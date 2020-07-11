open Core

let frame_count = 4096

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
  Exn.protect ~f:(fun () -> f mem umem fill comp) ~finally:(fun () -> Xsk.Umem.delete umem)
;;

let complete_tx addrs addrs_len sock fd tx fq cq =
  if Xsk.Tx_queue.needs_wakeup tx then (
    Xsk.Socket.wakeup_kernel_with_sendto sock
  );

  let completed = Xsk.Comp_queue.consume cq addrs ~pos:0 ~nb:addrs_len in
  if (completed <> 0) then (
    let filled = ref (Xsk.Fill_queue.produce_and_wakeup_kernel fq fd addrs ~pos:0 ~nb:completed) in
    while !filled <> completed do
      filled := (Xsk.Fill_queue.produce_and_wakeup_kernel fq fd addrs ~pos:0 ~nb:completed)
    done;
  )
;;

let swap_mac_tmp = Bigstring.create 6

let swap_mac mem addr =
  Bigstring.unsafe_blit ~src:mem ~src_pos:addr ~dst:swap_mac_tmp ~dst_pos:0 ~len:6;
  Bigstring.unsafe_blit ~src:mem ~src_pos:(addr + 6) ~dst:mem ~dst_pos:addr ~len:6;
  Bigstring.unsafe_blit ~src:swap_mac_tmp ~src_pos:0 ~dst:mem ~dst_pos:(addr + 6) ~len:6

let do_fwd
    mem
    fill
    comp
    socket
    rx
    tx
    frame_size
  =
  (* Populate the fill queue *)
  let addrs = Array.init frame_count ~f:(fun i -> i * frame_size) in
  let filled = Xsk.Fill_queue.produce fill addrs ~pos:0 ~nb:frame_count in
  let batch_size = 64 in
  let descs = Array.init batch_size ~f:(fun (_ : int) -> Xsk.Desc.create ()) in
  if filled <> frame_count
  then (
    let error_string =
      Printf.sprintf
        "Could not initialize fill queue. Filled %d expected %d"
        filled
        frame_count
    in
    Or_error.error_string error_string)
  else (
    let fd = Xsk.Socket.fd socket in
    let rec fwd_loop () =
      complete_tx addrs frame_count socket fd tx fill comp;
      match Xsk.Rx_queue.poll_and_consume rx fd 1000 descs ~pos:0 ~nb:frame_count with
      | None -> fwd_loop ()
      | Some rcvd -> (
        for i = 0 to rcvd - 1 do
          swap_mac mem (Array.unsafe_get descs i).addr;
        done;
        let sent = ref 0 in
        while !sent <> rcvd do
          sent := Xsk.Tx_queue.produce_and_wakeup_kernel tx fd descs ~pos:0 ~nb:frame_count;
        done
      )
    in
    Or_error.return (fwd_loop ()))
;;

let fwd bind_flags xdp_flags interface queue frame_size =
  with_umem frame_size ~f:(fun mem umem fill comp ->
      with_socket bind_flags xdp_flags interface queue umem ~f:(fun socket rx tx ->
          do_fwd mem fill comp socket rx tx frame_size))
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

let command =
  Command.basic
    ~summary:""
    Command.Let_syntax.(
      let open Command.Param in
      let%map interface = flag "-d" (required string) ~doc:"The device"
      and queue = flag "-q" (required int) ~doc:"The flag"
      and frame_size = flag "-f" (optional_with_default 2048 int) ~doc:"Frame size"
      and zero_copy =
        flag "-z" (no_arg_some Xsk.Bind_flag.XDP_ZEROCOPY) ~doc:"Zero copy mode"
      and needs_wakeup =
        flag
          "-w"
          (no_arg_some Xsk.Bind_flag.XDP_USE_NEED_WAKEUP)
          ~doc:"Use the needs wake up flag"
      in
      fun () ->
        let bf = make_bind_flags zero_copy needs_wakeup in
        let xdpf = make_xdp_flags None in
        match fwd bf xdpf interface queue frame_size with
        | Error _ -> ()
        | Ok () -> ())

;;

let () = Command.run command
