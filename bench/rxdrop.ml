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
  Exn.protect ~f:(fun () -> f umem fill comp) ~finally:(fun () -> Xsk.Umem.delete umem)
;;

let unsafe_iteri_upto a upto ~f =
  let rec loop i =
    if i = upto
    then ()
    else (
      f i (Array.unsafe_get a i);
      loop (i + 1))
  in
  loop 0
;;

let do_rx_drop
    (_ : Xsk.Umem.t)
    fill
    (_ : Xsk.Comp_queue.t)
    socket
    rx
    (_ : Xsk.Tx_queue.t)
    frame_size
    count
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
    let rec loop remaining =
      if remaining = 0 then () else (
      match Xsk.Rx_queue.poll_and_consume rx fd 1000 descs ~pos:0 ~nb:batch_size with
      | None -> loop remaining
      | Some rcvd ->
        Stdio.print_endline "got something!";
        unsafe_iteri_upto descs rcvd ~f:(fun i desc -> Array.unsafe_set addrs i desc.addr);
        let filled =
          ref (Xsk.Fill_queue.produce_and_wakeup_kernel fill fd addrs ~pos:0 ~nb:rcvd)
        in
        while !filled <> rcvd do
          filled := Xsk.Fill_queue.produce_and_wakeup_kernel fill fd addrs ~pos:0 ~nb:rcvd
        done;
        loop (remaining - 1))
    in
    let tick = Time_ns.now () in
    (loop count : unit);
    let tock = Time_ns.now () in
    let dur = Time_ns.diff tock tick |> Time_ns.Span.to_string_hum in
    Stdio.printf "Dropped %d frames in %s" count dur;
    Or_error.return "done")
;;

let rxdrop bind_flags xdp_flags interface queue frame_size cnt =
  with_umem frame_size ~f:(fun umem fill comp ->
      with_socket bind_flags xdp_flags interface queue umem ~f:(fun socket rx tx ->
          do_rx_drop umem fill comp socket rx tx frame_size cnt))
;;

let tx (_ : string) (_ : int) (_ : int) = ()

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
      and cnt =
        flag "-c" (optional_with_default 1_000_000 int) ~doc:"n How many packets to receive"
      in
      fun () ->
        let bf = make_bind_flags zero_copy needs_wakeup in
        let xdpf = make_xdp_flags None in
        rxdrop bf xdpf interface queue frame_size cnt
        |> Or_error.sexp_of_t String.sexp_of_t
        |> Stdio.eprint_s)
;;

let () = Command.run command
