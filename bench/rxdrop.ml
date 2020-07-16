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

module Hist = struct
  let bins = 2 lsl 4
  let bin_mask = bins - 1
  let bin_size = 1024 * 1024

  type t =
    { hist : Time_stamp_counter.t Array.t
    ; mutable cnt : int
    ; mutable bin : int
    }

  let create () =
    let hist = Array.init bins ~f:(fun (_ : int) -> Time_stamp_counter.zero) in
    { hist; cnt = 0; bin = 0 }
  ;;

  let incr t amount =
    if t.cnt + amount >= bin_size
    then (
      t.cnt <- t.cnt + amount - bin_size;
      let tsc = Time_stamp_counter.now () in
      Array.unsafe_set t.hist t.bin tsc;
      t.bin <- (t.bin + 1) land bin_mask)
    else t.cnt <- t.cnt + amount
  ;;

  let print t =
    match Array.find t.hist ~f:(fun t -> Time_stamp_counter.(equal t zero)) with
    | Some _ -> Stdio.printf "Not enough samples"
    | None ->
      let max =
        Array.fold t.hist ~init:Time_stamp_counter.zero ~f:(fun accum t ->
            Time_stamp_counter.max accum t)
      in
      let min =
        Array.fold t.hist ~init:(Time_stamp_counter.now ()) ~f:(fun accum t ->
            Time_stamp_counter.min accum t)
      in
      let dur = Time_stamp_counter.diff max min in
      let s =
        Time_stamp_counter.(Span.to_ns ~calibrator:(Lazy.force calibrator) dur)
        |> Int63.to_int_exn
        |> Int.to_float |> (/.) 1_000_000_000.0 
      in
      let p = bin_size * (Array.length t.hist - 2) in
      Stdio.printf "[rxdrop] %d (packets) %f (s) %f (pps)\n" p s (Float.of_int p /. s)
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
  let hist = Hist.create () in
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
      (* Every 1024 * 1024 frames store the current time *)
      if cnt >= upto
      then ()
      else (
        match Xsk.Rx_queue.consume rx descs ~pos:0 ~nb:batch_size with
        | 0 ->
          if Xsk.Fill_queue.needs_wakeup fill
          then Xsk.Socket.wakeup_kernel_with_sendto socket;
          drop_loop cnt
        | rcvd ->
          if rcvd < 0 then failwith "fuckup";
          for i = 0 to rcvd - 1 do
            let desc = Array.unsafe_get descs i in
            Array.unsafe_set addrs i desc.addr;
            if desc.len >= 8
            then ignore (Bigstring.unsafe_get_int64_le_trunc mem ~pos:desc.addr : int)
          done;
          Hist.incr hist rcvd;
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
    Hist.print hist;
    Or_error.return ())
;;

let rxdrop bind_flags xdp_flags interface queue frame_size cnt =
  with_umem frame_size ~f:(fun mem umem fill comp ->
      with_socket bind_flags xdp_flags interface queue umem ~f:(fun socket rx tx ->
          do_rx_drop mem umem fill comp socket rx tx frame_size cnt))
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
        flag
          "-c"
          (optional_with_default 1_000_000 int)
          ~doc:"n How many packets to receive"
      in
      fun () ->
        let bf = make_bind_flags zero_copy needs_wakeup in
        let xdpf = make_xdp_flags None in
        rxdrop bf xdpf interface queue frame_size cnt
        |> Or_error.sexp_of_t Unit.sexp_of_t
        |> Stdio.eprint_s)
;;

let () = Command.run command
