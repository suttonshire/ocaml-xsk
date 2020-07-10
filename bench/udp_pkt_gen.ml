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

type%cstruct udp4 =
  { src_port : uint16_t
  ; dst_port : uint16_t
  ; len : uint16_t
  ; csum : uint16_t
  }
[@@big_endian]

(** * [mac_of_string str] parses the mac address [str] into a format suitable for the *
    ethernet cstruct. Throws if [str] does not have the format XX:XX:XX:XX:XX:XX *)
let mac_of_string str =
  str
  |> String.split ~on:':'
  |> fun l ->
  if List.length l <> 6
  then raise (Invalid_argument "invalid mac address")
  else
    l
    |> List.map ~f:(fun s -> String.concat [ "0x"; s ])
    |> List.map ~f:Int.of_string
    |> List.map ~f:Char.of_int_exn
    |> String.of_char_list
;;

(** * [ip_of_string str] parses the ip address [str] into a format suitable for the *
    ethernet cstruct. Throws if [str] does not have the format XXX.XXX.XXX.XXX.XXX.XXX *)
let ip_of_string str =
  str
  |> String.split ~on:'.'
  |> fun l ->
  if List.length l <> 4
  then raise (Invalid_argument "invalid ip address")
  else
    l |> List.map ~f:Int.of_string |> List.map ~f:Char.of_int_exn |> String.of_char_list
;;

let set_pkt ~src_mac ~dst_mac ~src_ip ~dst_ip pkt pos =
  (* Create the ethernet header *)
  let ether = Cstruct.create sizeof_ethernet in
  let src_ether_addr = mac_of_string src_mac in
  let dst_ether_addr = mac_of_string dst_mac in
  set_ethernet_src src_ether_addr 0 ether;
  set_ethernet_dst dst_ether_addr 0 ether;
  set_ethernet_ethertype ether 0x800;
  (* Create the IP header *)
  let ip = Cstruct.create sizeof_ipv4 in
  let src_ip_addr = ip_of_string (Host_and_port.host src_ip) in
  let dst_ip_addr = ip_of_string (Host_and_port.host dst_ip) in
  set_ipv4_hlen_version ip 0x45;
  set_ipv4_tos ip 0;
  set_ipv4_len ip 20;
  set_ipv4_id ip 0;
  set_ipv4_off ip 0;
  set_ipv4_ttl ip 255;
  set_ipv4_proto ip 17;
  set_ipv4_csum ip 0;
  set_ipv4_dst dst_ip_addr 0 ip;
  set_ipv4_src src_ip_addr 0 ip;
  let csum = ref 0xFFFF in
  for i = 0 to 9 do
    csum := !csum + Cstruct.BE.get_uint16 ip (2 * i);
    if !csum > 0xFFFF then csum := !csum - 0xFFFF
  done;
  csum := lnot !csum;
  set_ipv4_csum ip !csum;
  (* Create the UDP header *)
  let udp = Cstruct.create sizeof_udp4 in
  let src_port = Host_and_port.port src_ip in
  let dst_port = Host_and_port.port dst_ip in
  let data_len = 8 in
  set_udp4_src_port udp src_port;
  set_udp4_dst_port udp dst_port;
  set_udp4_len udp (sizeof_udp4 + data_len);
  (* We don't set the csum for now *)
  set_udp4_csum udp 0;
  (* Create the data *)
  let buf = Cstruct.create data_len in
  Cstruct.LE.set_uint64 buf 0 (Int64.of_int pos);
  let data = Cstruct.concat [ ether; ip; udp; buf ] in
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

let send_batch batch_size frame_size send txq fd descs =
  for i = 0 to batch_size - 1 do
    let pos = (send + i) land (frame_count - 1) in
    descs.(i).Xsk.Desc.addr <- pos * frame_size
  done;
  let sent = ref 0 in
  while !sent = 0 do
    sent := Xsk.Tx_queue.produce_and_wakeup_kernel txq fd descs ~pos:0 ~nb:batch_size
  done;
  !sent
;;

(* The UMEM is arranged as an array of fixed sized buffers. There are [frame_count] buffers
 * each [frame_size] bytes in length. We maintain two pointers into the UMEM, [send] and
 * [consume]. [send] points to the next UMEM frame that should be added to the TX queue.
 * It's value is equal to the number of frames that have already been transmitted.
 * [consumed] points to the next UMEM frame that should be consumed from the Completion
 * queue. When [consume = send] is true then every frame that has been transmitted has
 * been consumed from the completion queue. [send - consume] is the number of frames
 * that we expect to consume from the completion queue. 
 *  
 * [send] and [consumed] are implemented as monotonically incrementing counters. To
 * convert the counter to a UMEM frame index take (sent % frame_cnt). 
 *  _________      _________
 * |__|__|__| --- |__|__|__|
 *  ^              ^ 
 *  consume        send
 * 
 * We stop transmitting when [send = cnt] is true. We exit when every packet that has
 * been sent is consumed
 *)
let tx cnt frame_size cq socket txq =
  Stdio.printf "Starting pkt get test\n";
  let fd = Xsk.Socket.fd socket in
  let max_batch_size = Int.min (Int.min frame_count cnt) 64 in
  let descs =
    Array.init max_batch_size ~f:(fun i ->
        let desc = Xsk.Desc.create () in
        desc.addr <- frame_size * i;
        desc.len <- 128;
        desc)
  in
  let addrs = Array.create ~len:max_batch_size 0 in
  let rec loop sent consumed =
    (* If we've sent everything and consumed everything we are done *)
    if sent >= cnt && consumed >= sent
    then ()
    else if (* Wait for the socket to be writeable *)
            not (Xsk.Socket.pollout socket 100)
    then loop sent consumed
    else (
      (* [tx_batch_size] is not more than the number of free frames remaining in the umem
       * and not more than the [max_batch_size]
       *)
      let tx_batch_size = Int.min max_batch_size (frame_count - (sent - consumed)) in
      let sent =
        (* If [tx_batch_size = 0] then we have transmitted every available umem frame and
         * need to wait for frames to come back from the completion queue *)
        if sent < cnt && tx_batch_size <> 0
        then sent + send_batch tx_batch_size frame_size sent txq fd descs
        else (
          (* Can't send anything. Ping the kernel just in case *)
          Xsk.Socket.wakeup_kernel_with_sendto socket;
          sent)
      in
      let consumed0 = Xsk.Comp_queue.consume cq addrs ~pos:0 ~nb:max_batch_size in
      loop sent (consumed + consumed0))
  in
  let tick = Time_ns.now () in
  loop 0 0;
  let tock = Time_ns.now () in
  let dur = Time_ns.diff tock tick in
  Stdio.printf "Sent %d packets in %s seconds" cnt (Time_ns.Span.to_string_hum dur)
;;

let make_xdp_flags mode =
  match mode with
  | Some flag -> [ flag; Xsk.Xdp_flag.XDP_FLAGS_UPDATE_IF_NOEXIST ]
  | None -> [ Xsk.Xdp_flag.XDP_FLAGS_SKB_MODE; Xsk.Xdp_flag.XDP_FLAGS_UPDATE_IF_NOEXIST ]
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
      let%map interface = flag "-d" (required string) ~doc:"device Device to transmit on"
      and queue = flag "-q" (required int) ~doc:"queue_id Interface queue to transmit on"
      and frame_size =
        flag "-f" (optional_with_default 2048 int) ~doc:"n Size of each frame in the umem"
      and src_ip =
        flag
          "-sip"
          (required host_and_port)
          ~doc:
            "source_ip Source host and port to use in the UDP and IP headers. Format \
             XXX.XXX.XXX.XXX:XXXX"
      and dst_ip =
        flag
          "-dip"
          (required host_and_port)
          ~doc:
            "destination_ip Destination host and port to use in the UDP and IP headers. \
             Format XXX.XXX.XXX.XXX:XXXX"
      and src_mac =
        flag
          "-smac"
          (required string)
          ~doc:
            "source_mac Source mac address to use in the ethernet headers. Format \
             XX:XX:XX:XX:XX:XX"
      and dst_mac =
        flag
          "-dmac"
          (required string)
          ~doc:
            "destination_mac Destination mac address to use in the ethernet headers. \
             Format XX:XX:XX:XX:XX:XX"
      and zero_copy =
        flag "-z" (no_arg_some Xsk.Bind_flag.XDP_ZEROCOPY) ~doc:"Zero copy mode"
      and needs_wakeup =
        flag
          "-w"
          (no_arg_some Xsk.Bind_flag.XDP_USE_NEED_WAKEUP)
          ~doc:"Use the needs wake up flag"
      and cnt =
        flag "-c" (optional_with_default 1_000_000 int) ~doc:"n How many packets to send"
      in
      let xdp_flags =
        match zero_copy with
        | None -> make_xdp_flags None
        | Some _ -> make_xdp_flags (Some Xsk.Xdp_flag.XDP_FLAGS_DRV_MODE)
      in
      let bind_flags = make_bind_flags zero_copy needs_wakeup in
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
