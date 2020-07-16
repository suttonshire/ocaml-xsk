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
      ~finally:(fun () ->
        Unix.close fd;
        Unix.unlink tmp_filename)
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

module Sampler = struct
  (** * Store throughput samples for each bin of 1024*1024 packets. We just store 16
      samples. *)
  let bins = 2 lsl 4

  let bin_mask = bins - 1
  let bin_size = 1024 * 1024

  type t =
    { hist : Time_stamp_counter.Span.t Array.t
    ; mutable cnt : int
    ; mutable bin : int
    ; mutable last_time : Time_stamp_counter.t
    }

  let create () =
    let hist =
      Array.init bins ~f:(fun (_ : int) -> Time_stamp_counter.Span.of_int_exn 0)
    in
    { hist; cnt = 0; bin = 0; last_time = Time_stamp_counter.now () }
  ;;

  let incr t amount =
    (* Every 1024 * 1024 frames store the delta t *)
    if t.cnt + amount >= bin_size
    then (
      t.cnt <- t.cnt + amount - bin_size;
      let tsc = Time_stamp_counter.now () in
      let diff = Time_stamp_counter.diff tsc t.last_time in
      t.last_time <- tsc;
      Array.unsafe_set t.hist t.bin diff;
      t.bin <- (t.bin + 1) land bin_mask)
    else t.cnt <- t.cnt + amount
  ;;

  let print t =
    match
      Array.find t.hist ~f:(fun t -> Time_stamp_counter.Span.(equal t (of_int_exn 0)))
    with
    | Some _ -> Stdio.printf "Not enough samples"
    | None ->
      let max_dt =
        Array.fold t.hist ~init:t.hist.(0) ~f:(fun accum t ->
            Time_stamp_counter.Span.max accum t)
        |> Time_stamp_counter.Span.to_ns
             ~calibrator:(Lazy.force Time_stamp_counter.calibrator)
      in
      let min_dt =
        Array.fold t.hist ~init:t.hist.(0) ~f:(fun accum t ->
            Time_stamp_counter.Span.min accum t)
        |> Time_stamp_counter.Span.to_ns
             ~calibrator:(Lazy.force Time_stamp_counter.calibrator)
      in
      let max_pps = Int.to_float bin_size /. (Int63.to_float min_dt /. 1000000000.0) in
      let min_pps = Int.to_float bin_size /. (Int63.to_float max_dt /. 1000000000.0) in
      Stdio.printf "[rxdrop] max: %f (pps) min: %f (pps) \n" max_pps min_pps
  ;;
end

let do_rx_drop
    mem
    (_ : Xsk.Umem.t)
    fill
    (_ : Xsk.Comp_queue.t)
    socket
    rx
    (_ : Xsk.Tx_queue.t)
    frame_size
    upto
  =
  (* Populate the fill queue *)
  let addrs = Array.init frame_count ~f:(fun i -> i * frame_size) in
  let fd = Xsk.Socket.fd socket in
  let filled =
    Xsk.Fill_queue.produce_and_wakeup_kernel fill fd addrs ~pos:0 ~nb:frame_count
  in
  let batch_size = 64 in
  let descs = Array.init frame_count ~f:(fun (_ : int) -> Xsk.Desc.create ()) in
  let hist = Sampler.create () in
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
    let rec drop_loop cnt =
      if cnt >= upto
      then ()
      else (
        match Xsk.Rx_queue.consume rx descs ~pos:0 ~nb:batch_size with
        | 0 ->
          if Xsk.Fill_queue.needs_wakeup fill
          then Xsk.Socket.wakeup_kernel_with_sendto socket;
          drop_loop cnt
        | rcvd ->
          if rcvd < 0 then failwith "oh dear";
          for i = 0 to rcvd - 1 do
            let desc = Array.unsafe_get descs i in
            Array.unsafe_set addrs i desc.addr;
            if desc.len >= 8
               (* Load a value from the frame to make sure we go all the way through the
                  LLC *)
            then ignore (Bigstring.unsafe_get_int64_le_trunc mem ~pos:desc.addr : int)
          done;
          Sampler.incr hist rcvd;
          let filled =
            ref (Xsk.Fill_queue.produce_and_wakeup_kernel fill fd addrs ~pos:0 ~nb:rcvd)
          in
          while !filled <> rcvd do
            filled
              := Xsk.Fill_queue.produce_and_wakeup_kernel fill fd addrs ~pos:0 ~nb:rcvd
          done;
          drop_loop (cnt + rcvd))
    in
    (drop_loop 0 : unit);
    Sampler.print hist;
    Or_error.return ())
;;

let rxdrop bind_flags xdp_flags interface queue frame_size cnt =
  with_umem frame_size ~f:(fun mem umem fill comp ->
      with_socket bind_flags xdp_flags interface queue umem ~f:(fun socket rx tx ->
          do_rx_drop mem umem fill comp socket rx tx frame_size cnt))
;;

let make_flags zero_copy needs_wakeup =
  (* Default bind flags are [ XDP_COPY ] *)
  match zero_copy, needs_wakeup with
  | None, None -> [ Xsk.Bind_flag.XDP_COPY ], [ Xsk.Xdp_flag.XDP_FLAGS_SKB_MODE ]
  | Some zc, None -> [ zc ], [ Xsk.Xdp_flag.XDP_FLAGS_DRV_MODE ]
  | None, Some nw -> [ nw; Xsk.Bind_flag.XDP_COPY ], [ Xsk.Xdp_flag.XDP_FLAGS_SKB_MODE ]
  | Some zc, Some nw -> [ zc; nw ], [ Xsk.Xdp_flag.XDP_FLAGS_DRV_MODE ]
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
        flag
          "-c"
          (optional_with_default 1_000_000 int)
          ~doc:"n How many packets to receive"
      in
      fun () ->
        let bf, xdpf = make_flags zero_copy needs_wakeup in
        match rxdrop bf xdpf interface queue frame_size cnt with
        | Error e -> Error.to_string_hum e |> Stdio.prerr_endline
        | Ok _ -> ())
;;

let () = Command.run command
