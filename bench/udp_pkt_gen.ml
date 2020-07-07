open Core

let frame_count = 4096

type%cstruct ethernet =
  { dst : uint8_t [@len 6]
  ; src : uint8_t [@len 6]
  ; ethertype : uint16_t
  }
[@@big_endian]

type%cstruct ipv4 =
  { hlen_version : uint8_t
  ; tos : uint8_t
  ; len : uint16_t
  ; id : uint16_t
  ; off : uint16_t
  ; ttl : uint8_t
  ; proto : uint8_t
  ; csum : uint16_t
  ; src : uint8_t [@len 4]
  ; dst : uint8_t [@len 4]
  }
[@@big_endian]

let mac_of_string str =
  str
  |> String.split ~on:':'
  |> List.map ~f:Int.of_string
  |> List.map ~f:Char.of_int_exn
  |> String.of_char_list
;;

let ip_of_string str =
  str
  |> String.split ~on:'.'
  |> List.map ~f:Int.of_string
  |> List.map ~f:Char.of_int_exn
  |> String.of_char_list
;;

let set_pkt ~src_mac ~dst_mac ~src_ip ~dst_ip pkt pos =
  let ether = Cstruct.create sizeof_ethernet in
  let src_ether_addr = mac_of_string src_mac in
  set_ethernet_src src_ether_addr 0 ether;
  let dst_ether_addr = mac_of_string dst_mac in
  set_ethernet_dst dst_ether_addr 0 ether;
  set_ethernet_ethertype ether 0x1234;
  let ip = Cstruct.create sizeof_ipv4 in
  let src_ip_addr = ip_of_string (Host_and_port.host src_ip) in
  let dst_ip_addr = ip_of_string (Host_and_port.host dst_ip) in
  set_ipv4_dst dst_ip_addr 0 ip;
  set_ipv4_src src_ip_addr 0 ip;
  set_ipv4_hlen_version ip 0x45;
  set_ipv4_tos ip 0;
  set_ipv4_len ip 40;
  (* TODO: We should derive this ourselves *)
  set_ipv4_csum ip 0xbad7;
  let data = Cstruct.concat [ ether; ip ] in
  Base_bigstring.From_bytes.blit
    ~src:(Cstruct.to_bytes data)
    ~src_pos:0
    ~dst:pkt
    ~dst_pos:pos
    ~len:(Cstruct.len data)
;;

let with_socket bind_flags xdp_flags interface queue umem ~f =
  let config =
    Xsk.Socket.
      { Config.default with
        rx_size = frame_count
      ; tx_size = frame_count
      ; xdp_flags
      ; bind_flags
      }
  in
  let socket, rx, tx = Xsk.Socket.create interface queue umem config in
  Exn.protect ~f:(fun () -> f socket rx tx) ~finally:(fun () -> Xsk.Socket.delete socket)
;;

let with_umem frame_size ~f =
  let tmp_filename = Filename.temp_file ~in_dir:"/tmp" "bench" "xsk" in
  let fd = Unix.openfile ~mode:[ Unix.O_RDWR ] tmp_filename in
  let mem =
    Exn.protect
      ~f:(fun () -> Bigstring.map_file ~shared:true fd (frame_count * frame_size))
      ~finally:(fun () -> Unix.unlink tmp_filename)
  in
  let config =
    Xsk.Umem.
      { Config.default with frame_size; fill_size = frame_count; comp_size = frame_count }
  in
  let umem, fill, comp = Xsk.Umem.create mem (Bigstring.length mem) config in
  Exn.protect
    ~f:(fun () -> f mem umem fill comp)
    ~finally:(fun () -> Xsk.Umem.delete umem)
;;

let tx cnt frame_size comp_queue socket txq =
  let fd = Xsk.Socket.fd socket in
  let batch_size = Int.min 64 cnt in
  let descs =
    Array.init batch_size ~f:(fun i ->
        let desc = Xsk.Desc.create () in
        desc.addr <- frame_size * i;
        desc.len <- 128;
        desc)
  in
  let addrs = Array.create ~len:batch_size 0 in
  let sent = Xsk.Tx_queue.produce_and_wakeup_kernel txq fd descs ~pos:0 ~nb:batch_size in
  let rec loop to_consume to_send =
    (* Consuming from the Completion queue lags sending on the Tx queue. If there's
       nothing to consume, we're done *)
    if to_consume <= 0
    then ()
    else (
      let consumed = Xsk.Comp_queue.consume comp_queue addrs ~pos:0 ~nb:to_consume in
      for i = 0 to consumed do
        (Array.unsafe_get descs i).addr <- Array.unsafe_get addrs i
      done;
      let to_consume = to_consume - consumed in
      (* If there is less than a batch size outstanding and there is more to send *)
      if to_consume < batch_size && to_send > 0
      then (
        let sent =
          Xsk.Tx_queue.produce_and_wakeup_kernel txq fd descs ~pos:0 ~nb:consumed
        in
        loop (to_consume + sent) (to_send - sent))
      else ())
  in
  loop sent (cnt - sent)
;;

let make_xdp_flags mode =
  match mode with
  | Some flag -> [ flag; Xsk.Xdp_flag.XDP_FLAGS_UPDATE_IF_NOEXIST ]
  | None -> [ Xsk.Xdp_flag.XDP_FLAGS_DRV_MODE; Xsk.Xdp_flag.XDP_FLAGS_UPDATE_IF_NOEXIST ]
;;

let make_bind_flags zero_copy needs_wakeup =
  (* Default bind flags are [ XDP_COPY ] *)
  match zero_copy, needs_wakeup with
  | None, None -> [ Xsk.Bind_flag.XDP_COPY ]
  | Some zc, None -> [ zc ]
  | None, Some nw -> [ nw ]
  | Some zc, Some nw -> [ zc; nw ]
;;

let populate_frames ~mem ~frame_size ~src_mac ~dst_mac ~src_ip ~dst_ip =
  let rec loop pos =
    if pos >= Base_bigstring.length mem
    then ()
    else (
      set_pkt ~src_mac ~dst_mac ~src_ip ~dst_ip mem pos;
      loop (pos + frame_size))
  in
  loop 0
;;

let command =
  Command.basic
    ~summary:"Generate a stream of 0 length UDP packets"
    Command.Let_syntax.(
      let open Command.Param in
      let%map interface = flag "-d" (required string) ~doc:"Device to transmit on"
      and queue = flag "-q" (required int) ~doc:"Transmit queue to transmit on"
      and frame_size =
        flag "-f" (optional_with_default 2048 int) ~doc:"Size of each frame in the umem"
      and src_ip =
        flag
          "-sip"
          (required host_and_port)
          ~doc:
            "Source host and port to use in the UDP and IP headers. Format \
             XXX.XXX.XXX.XXX:XXXX"
      and dst_ip =
        flag
          "-dip"
          (required host_and_port)
          ~doc:
            "Destination host and port to use in the UDP and IP headers. Format \
             XXX.XXX.XXX.XXX:XXXX"
      and src_mac =
        flag
          "-smac"
          (required string)
          ~doc:"Src mac address to use in the ethernet headers. Format XX:XX:XX:XX:XX:XX"
      and dst_mac =
        flag "-dmac" (required string) ~doc:"Dst mac address. Format XX:XX:XX:XX:XX:XX"
      and zero_copy =
        flag "-z" (no_arg_some Xsk.Bind_flag.XDP_ZEROCOPY) ~doc:"Zero copy mode"
      and needs_wakeup =
        flag
          "-w"
          (no_arg_some Xsk.Bind_flag.XDP_USE_NEED_WAKEUP)
          ~doc:"Use the needs wake up flag"
      and cnt =
        flag "-c" (optional_with_default 1_000_000 int) ~doc:"How many packets to send"
      in
      let bind_flags = make_bind_flags zero_copy needs_wakeup in
      let xdp_flags = make_xdp_flags None in
      fun () ->
        with_umem frame_size ~f:(fun mem umem (_ : Xsk.Fill_queue.t) comp ->
            with_socket
              bind_flags
              xdp_flags
              interface
              queue
              umem
              ~f:(fun socket (_ : Xsk.Rx_queue.t) txq ->
                populate_frames ~mem ~frame_size ~src_mac ~dst_mac ~src_ip ~dst_ip;
                tx cnt frame_size comp socket txq)))
;;

let () = Command.run command
