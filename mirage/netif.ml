open Mirage_net

module Batch = struct
  type t =
    { addrs : int array
    ; descs : Xsk.Desc.t array
    ; size : int
    }

  let create size =
    let addrs = Array.init size (fun _ -> -1) in
    let descs = Array.init size (fun _ -> Xsk.Desc.create ()) in
    { addrs; descs; size }
  ;;
end

module Addrpool = struct
  exception Double_free

  type t =
    { free_addrs : int array
    ; mutable free : int
    ; frame_cnt : int
    }

  let allocate t =
    if t.free = t.frame_cnt
    then None
    else (
      let addr = t.free_addrs.(t.free) in
      t.free <- succ t.free;
      Some addr)
  ;;

  let free t addr =
    if t.free = 0
    then raise Double_free
    else (
      t.free <- pred t.free;
      t.free_addrs.(t.free) <- addr)
  ;;

  let create addr_start ~frame_cnt ~frame_size =
    let free_addrs = Array.init frame_cnt (fun i -> addr_start + (i * frame_size)) in
    { free_addrs; free = 0; frame_cnt }
  ;;
end

type t =
  { umem : Xsk.Umem.t
  ; socket : Xsk.Socket.t
  ; rx_queue : Xsk.Rx_queue.t
  ; tx_queue : Xsk.Tx_queue.t
  ; comp_queue : Xsk.Comp_queue.t
  ; fill_queue : Xsk.Fill_queue.t
  ; rx_batch : Batch.t
  ; raw_mem : Cstruct.buffer
  ; cs_mem : Cstruct.t
  ; tx_batch : Batch.t
  ; addrpool : Addrpool.t
  ; lwt_fd : Lwt_unix.file_descr
  ; fd : Unix.file_descr
  ; quit : bool Lwt.t
  ; quit_resolver : bool Lwt.u
  ; stats : stats
  ; mac : Macaddr.t
  ; mtu : int
  }

type error =
  [ Net.error
  | `Tx_no_mem
  ]

let pp_error ppf = function
  | #Net.error as e -> Net.pp_error ppf e
  | `Tx_no_mem -> Fmt.pf ppf "xsk: no buffers available for packet transmit"
;;

(* Since we know we are running on linux we can get the mtu from /sys/class/net/<dev>/mtu *)
let read_mtu devname =
  let fn = Printf.sprintf "/sys/class/net/%s/mtu" devname in
  let%lwt f = Lwt_io.open_file ~mode:Lwt_io.Input fn in
  let%lwt mtu_string = Lwt_io.read_line f in
  mtu_string |> int_of_string |> Lwt.return
;;

let mtu t = t.mtu

(* Since we know we are running on Linux we can get the mac address from
   /sys/class/net/<dev>/address *)
let read_mac devname =
  let fn = Printf.sprintf "/sys/class/net/%s/address" devname in
  let%lwt f = Lwt_io.open_file ~mode:Lwt_io.Input fn in
  let%lwt mac_string = Lwt_io.read_line f in
  Macaddr.of_string_exn mac_string |> Lwt.return
;;

let ethernet_header_len = 14
let ethernet_header_vlan_len = 18
let mac t = t.mac
let reset_stats_counters t = Stats.reset t.stats
let get_stats_counters t = t.stats

let listen t ~header_size f =
  if header_size <> ethernet_header_len || header_size <> ethernet_header_vlan_len
  then (
    let%lwt () = Lwt_io.printf "listen invalid header_size %d\n" header_size in
    Lwt.return_error `Invalid_length)
  else (
    let rec loop rcvd =
      let poll = Lwt_unix.register_action Lwt_unix.Read t.lwt_fd (fun () -> false) in
      let%lwt quit = Lwt.choose [ poll; t.quit ] in
      if quit
      then Lwt.return ()
      else (
        let filled =
          if rcvd <> 0
          then Xsk.Fill_queue.produce t.fill_queue t.rx_batch.addrs ~pos:0 ~nb:rcvd
          else 0
        in
        let rcvd =
          if filled = rcvd
          then Xsk.Rx_queue.consume t.rx_queue t.rx_batch.descs ~pos:0 ~nb:t.rx_batch.size
          else 0
        in
        if rcvd <> 0
        then
          for i = 0 to pred rcvd do
            let desc = t.rx_batch.descs.(i) in
            let buf = Cstruct.create desc.len in
            Cstruct.blit t.cs_mem desc.addr buf 0 desc.len;
            t.rx_batch.addrs.(i) <- desc.addr;
            Lwt.async (fun () -> f buf)
          done;
        loop rcvd)
    in
    let%lwt () = loop 0 in
    Lwt.return_ok ())
;;

let write t ~size fill =
  if size < 0 || size > mtu t + ethernet_header_len
  then Lwt.return_error `Invalid_length
  else (
    if Xsk.Tx_queue.needs_wakeup t.tx_queue
    then Xsk.Socket.wakeup_kernel_with_sendto t.socket;
    let consumed =
      Xsk.Comp_queue.consume t.comp_queue t.tx_batch.addrs ~pos:0 ~nb:t.tx_batch.size
    in
    if consumed <> 0
    then
      for i = 0 to pred consumed do
        Addrpool.free t.addrpool t.tx_batch.addrs.(i)
      done;
    match Addrpool.allocate t.addrpool with
    | None -> Lwt.return_error `Tx_no_mem
    | Some addr ->
      let buf = Cstruct.of_bigarray ~off:addr ~len:size t.raw_mem in
      Cstruct.memset buf 0;
      let len = fill buf in
      let desc = t.tx_batch.descs.(0) in
      desc.len <- len;
      desc.addr <- addr;
      desc.options <- 0;
      let produced = ref 0 in
      while !produced <> 1 do
        produced
          := Xsk.Tx_queue.produce_and_wakeup_kernel
               t.tx_queue
               t.fd
               t.tx_batch.descs
               ~pos:0
               ~nb:1
      done;
      Lwt.return_ok ())
;;

let make_mem size =
  let mem_filename = Filename.temp_file ~temp_dir:"/tmp" "Netif_xsk" "mem" in
  let memfd =
    Unix.openfile mem_filename [ Unix.O_RDWR; Unix.O_CREAT; Unix.O_CLOEXEC ] 0600
  in
  Fun.protect
    ~finally:(fun () ->
      Unix.close memfd;
      Unix.unlink mem_filename)
    (fun () ->
      Unix.ftruncate memfd size;
      let mem = Unix.map_file memfd Bigarray.char Bigarray.c_layout true [| -1 |] in
      Bigarray.array1_of_genarray mem)
;;

let connect ?umem_config ?socket_config devname queue_id =
  let umem_config =
    Option.fold ~none:Xsk.Umem.Config.default ~some:(fun a -> a) umem_config
  in
  let socket_config =
    Option.fold ~none:Xsk.Socket.Config.default ~some:(fun a -> a) socket_config
  in
  (* Make memory large enough to have the Tx queue full and the Fill queue full *)
  let mem_size =
    umem_config.frame_size * (socket_config.tx_size + umem_config.fill_size)
  in
  let mem = make_mem mem_size in
  let cs_mem = Cstruct.of_bigarray mem in
  let umem, fill_queue, comp_queue = Xsk.Umem.create mem mem_size umem_config in
  let socket, rx_queue, tx_queue =
    try Xsk.Socket.create devname queue_id umem socket_config with
    | _ as e ->
      Xsk.Umem.delete umem;
      raise e
  in
  try
    let tx_batch = Batch.create 64 in
    let rx_batch = Batch.create 64 in
    (* The first socket_config.tx_size frames will be used for tx work *)
    let addrpool =
      Addrpool.create
        0
        ~frame_cnt:socket_config.tx_size
        ~frame_size:umem_config.frame_size
    in
    (* The next umem_config.fill_size will be used for rx work *)
    let rx_start_addr = socket_config.tx_size + umem_config.frame_size in
    for i = 0 to pred umem_config.fill_size do
      let addr = [| rx_start_addr + (i * umem_config.frame_size) |] in
      let produced = ref 0 in
      while !produced <> 1 do
        produced := Xsk.Fill_queue.produce fill_queue addr ~pos:0 ~nb:1
      done
    done;
    let%lwt mac = read_mac devname in
    let%lwt mtu = read_mtu devname in
    let quit, quit_resolver = Lwt.wait () in
    Lwt.return
      { umem
      ; socket
      ; rx_queue
      ; tx_queue
      ; comp_queue
      ; fill_queue
      ; rx_batch
      ; tx_batch
      ; raw_mem = mem
      ; cs_mem
      ; addrpool
      ; fd = Xsk.Socket.fd socket
      ; lwt_fd =
          Lwt_unix.of_unix_file_descr
            ~blocking:true
            ~set_flags:false
            (Xsk.Socket.fd socket)
      ; stats = Stats.create ()
      ; mac
      ; mtu
      ; quit
      ; quit_resolver
      }
  with
  | _ as e ->
    Xsk.Socket.delete socket;
    Xsk.Umem.delete umem;
    raise e
;;

let disconnect t =
  Lwt.wakeup t.quit_resolver true;
  Xsk.Socket.delete t.socket;
  Xsk.Umem.delete t.umem;
  Lwt.return ()
;;
