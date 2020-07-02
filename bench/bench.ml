open Core

let frame_count = 4096

let with_socket interface queue umem ~f =
  let config =
    Xsk.Socket.{ Config.default with rx_size = frame_count; tx_size = frame_count }
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
  let config = Xsk.Umem.{ Config.default with frame_size } in
  let umem, fill, comp = Xsk.Umem.create mem (Bigstring.length mem) config in
  Exn.protect ~f:(fun () -> f umem fill comp) ~finally:(fun () -> Xsk.Umem.delete umem)
;;

let unsafe_iteri_upto a upto ~f =
  let rec loop i =
    if i = upto then () else (
      f i (Array.unsafe_get a i);
      loop (i + 1)
    )
  in
  loop 0
;;

let do_rx_drop (_ : Xsk.Umem.t) fill (_ : Xsk.Comp_queue.t) socket rx (_ : Xsk.Tx_queue.t) frame_size =
  (* Populate the fill queue *)
  let addrs = Array.init frame_count ~f:(fun i -> i * frame_size) in
  let filled = Xsk.Fill_queue.produce fill addrs ~pos:0 ~nb:frame_count in
  let batch_size = 64 in
  let descs = Array.init batch_size ~f:(fun (_ : int) -> Xsk.Desc.create ()) in
  if filled <> frame_count then Or_error.return "Could not initialize fill queue" else (
    let fd = Xsk.Socket.fd socket in
    let rec loop () =
      match Xsk.Rx_queue.poll_and_consume rx fd 1000 descs ~pos:0 ~nb:0 with
      | None -> loop ()
      | Some rcvd -> (
        unsafe_iteri_upto descs rcvd ~f:(fun i desc -> 
        Array.unsafe_set addrs i desc.addr);
        let filled = ref (Xsk.Fill_queue.produce_and_wakeup_kernel fill fd addrs ~pos:0 ~nb:rcvd) in
        while !filled <> rcvd do
          filled := Xsk.Fill_queue.produce_and_wakeup_kernel fill fd addrs ~pos:0 ~nb:rcvd;
        done
      )
    in
    loop ();
    failwith "what"
  )
;;

let rxdrop interface queue frame_size =
  with_umem frame_size ~f:(fun umem fill comp -> 
    with_socket interface queue umem ~f:(fun socket rx tx -> 
      do_rx_drop umem fill comp socket rx tx frame_size
    )
  )
;;

let tx (_ : string) (_ : int) (_ : int) = ()

let command =
  Command.basic
    ~summary:""
    Command.Let_syntax.(
      let open Command.Param in
      let%map bench = anon ("bench" %: string)
      and interface = flag "-d" (required string) ~doc:"The device"
      and queue = flag "-q" (required int) ~doc:"The flag"
      and frame_size = flag "-f" (optional_with_default 2048 int) ~doc:"Frame size" in
      (fun () ->
        match bench with
        | "rxdrop" -> rxdrop interface queue frame_size |> Or_error.sexp_of_t String.sexp_of_t |> Stdio.eprint_s
        | "tx" -> tx interface queue frame_size
        | _ -> Stdio.printf "Invalid benchmark %s" bench))
;;

let () = Command.run command  