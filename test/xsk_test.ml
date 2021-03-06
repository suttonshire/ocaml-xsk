open Base
open Xsk

let print_endline = Stdio.print_endline
let script_dir = Sys.getenv_exn "XSK_TEST_DEPS_DIR"
let test_if_name = Sys.getenv_exn "XSK_TEST_INTF_NAME"
let test_if_mac = Sys.getenv_exn "XSK_TEST_INTF_MAC"
let echo_if_name = Sys.getenv_exn "XSK_ECHO_INTF_NAME"
let echo_if_mac = Sys.getenv_exn "XSK_ECHO_INTF_MAC"

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

(* Convert a string like XX:XX:XX:XX:XX:XX to a string that can be used in the ethernet
   cstruct *)
let mac_of_string str =
  str
  |> String.split ~on:':'
  |> List.map ~f:Int.of_string
  |> List.map ~f:Char.of_int_exn
  |> String.of_char_list
;;

let set_pkt pkt pos =
  let ether = Cstruct.create sizeof_ethernet in
  let src_ether_addr = mac_of_string test_if_mac in
  set_ethernet_src src_ether_addr 0 ether;
  let dst_ether_addr = mac_of_string echo_if_mac in
  set_ethernet_dst dst_ether_addr 0 ether;
  set_ethernet_ethertype ether 0x1234;
  let ip = Cstruct.create sizeof_ipv4 in
  set_ipv4_dst (String.of_char_list [ '\x00'; '\x00'; '\x00'; '\x00' ]) 0 ip;
  set_ipv4_src (String.of_char_list [ '\x00'; '\x00'; '\x00'; '\x00' ]) 0 ip;
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

(* Return a page-aligned bigarray *)
let make_mem frame_size frame_cnt =
  let tmp_file = Printf.sprintf "/tmp/%d.af_xdp_test.mem" (Random.bits ()) in
  let fd = Unix.openfile tmp_file [ Unix.O_CREAT; Unix.O_RDWR ] 0666 in
  Unix.ftruncate fd (frame_size * frame_cnt);
  let buf =
    Unix.map_file fd Bigarray.char Bigarray.c_layout true [| frame_size * frame_cnt |]
    |> Bigarray.array1_of_genarray
  in
  Unix.close fd;
  Unix.unlink tmp_file;
  buf
;;

let with_dev f =
  let setup_cmd =
    Printf.sprintf
      "%s/setup.sh %s %s %s %s %s"
      script_dir
      test_if_name
      test_if_mac
      echo_if_name
      echo_if_mac
      script_dir
  in
  let res = Unix.system setup_cmd in
  (match res with
  | Unix.WEXITED 0 -> ()
  | _ -> ());
  Exn.protect
    ~f:(fun () -> f test_if_name)
    ~finally:(fun () ->
      let teardown_cmd =
        Printf.sprintf "%s/teardown.sh %s %s" script_dir test_if_name echo_if_name
      in
      let res = Unix.system teardown_cmd in
      match res with
      | Unix.WEXITED 0 -> ()
      | _ -> ())
;;

let with_umem config frame_cnt f =
  let mem = make_mem config.Umem.Config.frame_size frame_cnt in
  let umem, fill, comp =
    Umem.create mem (config.Umem.Config.frame_size * frame_cnt) config
  in
  Exn.protect ~f:(fun () -> f mem umem fill comp) ~finally:(fun () -> Umem.delete umem)
;;

let with_socket umem ifname queue_id config f =
  let socket, rx, tx = Socket.create ifname queue_id umem config in
  Exn.protect ~f:(fun () -> f socket rx tx) ~finally:(fun () -> Socket.delete socket)
;;

let test_with ~frame_cnt ?umem_config ?socket_config f =
  let umem_config =
    umem_config
    |> function
    | None -> Umem.Config.default
    | Some x -> x
  in
  let socket_config =
    socket_config
    |> function
    | None -> Socket.Config.default
    | Some x -> x
  in
  with_dev (fun ifname ->
      with_umem umem_config frame_cnt (fun mem umem fill comp ->
          with_socket umem ifname 0 socket_config (fun socket rx tx ->
              f mem (umem, fill, comp) (socket, rx, tx))))
;;

(* Fill_queue tests *)
let%expect_test "Fill_queue.produce up to fill_size frames" =
  let umem_config = { Umem.Config.default with fill_size = 4; frame_size = 4096 } in
  test_with ~frame_cnt:8 ~umem_config (fun _ (_, fq, _) _ ->
      List.iter [ 1; 2; 3; 4; 5; 6 ] ~f:(fun i ->
          Stdio.printf "%d " (Fill_queue.produce fq [| i * 4096 |] ~pos:0 ~nb:1)));
  [%expect {| 1 1 1 1 0 0 |}]
;;

let%expect_test "Fill_queue.produce_and_wakeup_kernel up to fill_size frames" =
  let umem_config = { Umem.Config.default with fill_size = 4; frame_size = 4096 } in
  test_with ~frame_cnt:8 ~umem_config (fun _ (_, fq, _) (sock, _, _) ->
      List.iter [ 1; 2; 3; 4; 5; 6 ] ~f:(fun i ->
          let fd = Socket.fd sock in
          Stdio.printf
            "%d "
            (Fill_queue.produce_and_wakeup_kernel fq fd [| i * 4096 |] ~pos:0 ~nb:1)));
  [%expect {| 1 1 1 1 0 0 |}]
;;

let%expect_test "Fill_queue.produce a batch of fill_size frames into fill queue" =
  let umem_config = { Umem.Config.default with fill_size = 4; frame_size = 4096 } in
  test_with ~frame_cnt:4 ~umem_config (fun _ (_, fq, _) _ ->
      Stdio.printf "%d" (Fill_queue.produce fq [| 0; 4096; 8192; 12288 |] ~pos:0 ~nb:4));
  [%expect {| 4 |}]
;;

let%expect_test "Fill_queue.produce_and_wakeup_kernel a batch of fill_size frames into \
                 fill queue"
  =
  let umem_config = { Umem.Config.default with fill_size = 4; frame_size = 4096 } in
  test_with ~frame_cnt:4 ~umem_config (fun _ (_, fq, _) (sock, _, _) ->
      let fd = Socket.fd sock in
      Stdio.printf
        "%d"
        (Fill_queue.produce_and_wakeup_kernel
           fq
           fd
           [| 0; 4096; 8192; 12288 |]
           ~pos:0
           ~nb:4));
  [%expect {| 4 |}]
;;

let%expect_test "Fill_queue.produce returns the number of frames produced into the queue" =
  let umem_config = { Umem.Config.default with fill_size = 4; frame_size = 4096 } in
  test_with ~frame_cnt:8 ~umem_config (fun _ (_, fq, _) _ ->
      Stdio.printf
        "%d "
        (Fill_queue.produce fq [| 0; 4096; 8192; 12288; 16384 |] ~pos:0 ~nb:5);
      Stdio.printf
        "%d"
        (Fill_queue.produce fq [| 0; 4096; 8192; 12288; 16384 |] ~pos:0 ~nb:4));
  [%expect {| 0 4 |}]
;;

let%expect_test "Fill_queue.produce_and_wakeup_kernel returns the number of frames \
                 produced into the queue"
  =
  let umem_config = { Umem.Config.default with fill_size = 4; frame_size = 4096 } in
  test_with ~frame_cnt:8 ~umem_config (fun _ (_, fq, _) (sock, _, _) ->
      let fd = Socket.fd sock in
      Stdio.printf
        "%d "
        (Fill_queue.produce_and_wakeup_kernel
           fq
           fd
           [| 0; 4096; 8192; 12288; 16384 |]
           ~pos:0
           ~nb:5);
      Stdio.printf
        "%d"
        (Fill_queue.produce_and_wakeup_kernel
           fq
           fd
           [| 0; 4096; 8192; 12288; 16384 |]
           ~pos:0
           ~nb:4));
  [%expect {| 0 4 |}]
;;

let%expect_test "Fill_queue.produce raises index out of bounds" =
  let umem_config = { Umem.Config.default with fill_size = 4; frame_size = 4096 } in
  test_with ~frame_cnt:4 ~umem_config (fun _ (_, fq, _) _ ->
      Expect_test_helpers_base.show_raise (fun () ->
          Fill_queue.produce fq [||] ~pos:0 ~nb:1));
  [%expect {| (raised (Invalid_argument "index out of bounds")) |}]
;;

let%expect_test "Fill_queue.produce_and_wakeup raises index out of bounds" =
  let umem_config = { Umem.Config.default with fill_size = 4; frame_size = 4096 } in
  test_with ~frame_cnt:4 ~umem_config (fun _ (_, fq, _) (sock, _, _) ->
      Expect_test_helpers_base.show_raise (fun () ->
          let fd = Socket.fd sock in
          Fill_queue.produce_and_wakeup_kernel fq fd [||] ~pos:0 ~nb:1));
  [%expect {| (raised (Invalid_argument "index out of bounds")) |}]
;;

(* Tx_queue tests *)
let%expect_test "Tx_queue.produce up to tx_size frames into tx queue" =
  let umem_config = { Umem.Config.default with fill_size = 4; frame_size = 4096 } in
  let socket_config = { Socket.Config.default with rx_size = 4; tx_size = 4 } in
  test_with ~frame_cnt:8 ~umem_config ~socket_config (fun _ (_, _, _) (_, _, tx) ->
      List.iter [ 0; 1; 2; 3; 4; 5 ] ~f:(fun i ->
          Stdio.printf
            "%d "
            (Tx_queue.produce
               tx
               [| { addr = 4096 * i; len = 1; options = 0 } |]
               ~pos:0
               ~nb:1)));
  [%expect {| 1 1 1 1 0 0 |}]
;;

let%expect_test "Tx_queue.produce_and_wakeup_kernel more than tx queue size" =
  let umem_config = { Umem.Config.default with fill_size = 4; frame_size = 4096 } in
  let socket_config = { Socket.Config.default with rx_size = 4; tx_size = 4 } in
  test_with ~frame_cnt:8 ~umem_config ~socket_config (fun _ (_, _, _) (s, _, tx) ->
      List.iter [ 0; 1; 2; 3; 4; 5 ] ~f:(fun i ->
          Stdio.printf
            "%d "
            (Tx_queue.produce_and_wakeup_kernel
               tx
               (Socket.fd s)
               [| { addr = 4096 * i; len = 1; options = 0 } |]
               ~pos:0
               ~nb:1)));
  [%expect {| 1 1 1 1 1 1 |}]
;;

let%expect_test "Tx_queue.produce returns the number of frames produced" =
  let umem_config = { Umem.Config.default with fill_size = 4; frame_size = 4096 } in
  let socket_config = { Socket.Config.default with rx_size = 4; tx_size = 4 } in
  test_with ~frame_cnt:8 ~umem_config ~socket_config (fun _ (_, _, _) (_, _, tx) ->
      let bufs =
        Array.init 5 ~f:(fun i -> Desc.{ addr = 4096 * i; len = 0; options = 0 })
      in
      Stdio.printf "%d " (Tx_queue.produce tx bufs ~pos:0 ~nb:5);
      Stdio.printf "%d " (Tx_queue.produce tx bufs ~pos:0 ~nb:4));
  [%expect {| 0 4 |}]
;;

let%expect_test "Tx_queue.produce_and_wakeup_kernel returns the number of frames produced"
  =
  let umem_config = { Umem.Config.default with fill_size = 4; frame_size = 4096 } in
  let socket_config = { Socket.Config.default with rx_size = 4; tx_size = 4 } in
  test_with ~frame_cnt:8 ~umem_config ~socket_config (fun _ (_, _, _) (s, _, tx) ->
      let bufs =
        Array.init 5 ~f:(fun i -> Desc.{ addr = 4096 * i; len = 0; options = 0 })
      in
      Stdio.printf
        "%d "
        (Tx_queue.produce_and_wakeup_kernel tx (Socket.fd s) bufs ~pos:0 ~nb:5);
      Stdio.printf
        "%d "
        (Tx_queue.produce_and_wakeup_kernel tx (Socket.fd s) bufs ~pos:0 ~nb:4));
  [%expect {| 0 4 |}]
;;

let%expect_test "Tx_queue.produce/_and_wakeup_kernel raises if params are outside of \
                 array bounds"
  =
  let umem_config = { Umem.Config.default with fill_size = 4; frame_size = 4096 } in
  let socket_config = { Socket.Config.default with rx_size = 4; tx_size = 4 } in
  test_with ~frame_cnt:8 ~umem_config ~socket_config (fun _ (_, _, _) (s, _, tx) ->
      Expect_test_helpers_base.show_raise (fun () ->
          ignore (Tx_queue.produce tx [||] ~pos:0 ~nb:1 : int));
      Expect_test_helpers_base.show_raise (fun () ->
          ignore
            (Tx_queue.produce_and_wakeup_kernel tx (Socket.fd s) [||] ~pos:0 ~nb:1 : int)));
  [%expect
    {|
  (raised (Invalid_argument "index out of bounds"))
  (raised (Invalid_argument "index out of bounds")) |}]
;;

(* Comp_queue tests *)
let%expect_test "Comp_queue.consume returns zero when nothing is available" =
  let umem_config = { Umem.Config.default with fill_size = 4; frame_size = 4096 } in
  let socket_config = { Socket.Config.default with rx_size = 4; tx_size = 4 } in
  test_with ~frame_cnt:8 ~umem_config ~socket_config (fun _ (_, _, comp) (_, _, _) ->
      Stdio.printf "%d" (Comp_queue.consume comp [| 0 |] ~pos:0 ~nb:1));
  [%expect {| 0 |}]
;;

let%expect_test "Comp_queue.consume raises if params are outside of array bounds" =
  let umem_config = { Umem.Config.default with fill_size = 4; frame_size = 4096 } in
  let socket_config = { Socket.Config.default with rx_size = 4; tx_size = 4 } in
  test_with ~frame_cnt:8 ~umem_config ~socket_config (fun _ (_, _, comp) (_, _, _) ->
      Expect_test_helpers_base.show_raise (fun () ->
          ignore (Comp_queue.consume comp [||] ~pos:0 ~nb:1 : int)));
  [%expect {| (raised (Invalid_argument "index out of bounds")) |}]
;;

let%expect_test "Comp_queue.consume should be empty after tx failure" =
  let umem_config = { Umem.Config.default with fill_size = 4; frame_size = 4096 } in
  let socket_config = { Socket.Config.default with rx_size = 4; tx_size = 4 } in
  test_with ~frame_cnt:8 ~umem_config ~socket_config (fun _ (_, _, comp) (s, _, tx) ->
      let bufs =
        Array.init 5 ~f:(fun i -> Desc.{ addr = 4096 * i; len = 0; options = 0 })
      in
      let sent = Tx_queue.produce_and_wakeup_kernel tx (Socket.fd s) bufs ~pos:0 ~nb:5 in
      let completed = [| -1 |] in
      let consumed = Comp_queue.consume comp completed ~pos:0 ~nb:1 in
      Stdio.printf "%d %d %d" sent completed.(0) consumed);
  [%expect {| 0 -1 0 |}]
;;

let%expect_test "Comp_queue is filled after Tx" =
  let umem_config = { Umem.Config.default with fill_size = 4; frame_size = 4096 } in
  let socket_config = { Socket.Config.default with rx_size = 4; tx_size = 4 } in
  test_with ~frame_cnt:8 ~umem_config ~socket_config (fun _ (_, _, comp) (s, _, tx) ->
      let sent =
        Tx_queue.produce tx [| { addr = 0; len = 33; options = 0 } |] ~pos:0 ~nb:1
      in
      Socket.wakeup_kernel_with_sendto s;
      let completed = [| -1 |] in
      let consumed = Comp_queue.consume comp completed ~pos:0 ~nb:1 in
      Stdio.printf "%d %d %d" sent completed.(0) consumed);
  [%expect {| 1 0 1 |}]
;;

(* Rx_queue tests *)
let%expect_test "Rx_queue.consume returns zero when nothing is available" =
  let umem_config = { Umem.Config.default with fill_size = 4; frame_size = 4096 } in
  let socket_config = { Socket.Config.default with rx_size = 4; tx_size = 4 } in
  test_with ~frame_cnt:8 ~umem_config ~socket_config (fun _ (_, _, _) (_, rx, _) ->
      Stdio.printf
        "%d"
        (Rx_queue.consume rx [| Desc.{ addr = 0; len = 0; options = 0 } |] ~pos:0 ~nb:1));
  [%expect {| 0 |}]
;;

let%expect_test "Rx_queue.poll_and_consume returns zero when nothing is available" =
  let umem_config = { Umem.Config.default with fill_size = 4; frame_size = 4096 } in
  let socket_config = { Socket.Config.default with rx_size = 4; tx_size = 4 } in
  test_with ~frame_cnt:8 ~umem_config ~socket_config (fun _ (_, _, _) (sock, rx, _) ->
      let fd = Socket.fd sock in
      Rx_queue.poll_and_consume
        rx
        fd
        10
        [| Desc.{ addr = 0; len = 0; options = 0 } |]
        ~pos:0
        ~nb:1
      |> Option.sexp_of_t Int.sexp_of_t
      |> Stdio.print_s);
  [%expect {| () |}]
;;

let%expect_test "Rx_queue.consume raises if params are outside of array bounds" =
  let umem_config = { Umem.Config.default with fill_size = 4; frame_size = 4096 } in
  let socket_config = { Socket.Config.default with rx_size = 4; tx_size = 4 } in
  test_with ~frame_cnt:8 ~umem_config ~socket_config (fun _ (_, _, _) (_, rx, _) ->
      Expect_test_helpers_base.show_raise (fun () ->
          ignore (Rx_queue.consume rx [||] ~pos:0 ~nb:1 : int)));
  [%expect {| (raised (Invalid_argument "index out of bounds")) |}]
;;

let%expect_test "Rx_queue.poll_and_consume raises if params are outside of array bounds" =
  let umem_config = { Umem.Config.default with fill_size = 4; frame_size = 4096 } in
  let socket_config = { Socket.Config.default with rx_size = 4; tx_size = 4 } in
  test_with ~frame_cnt:8 ~umem_config ~socket_config (fun _ (_, _, _) (sock, rx, _) ->
      Expect_test_helpers_base.show_raise (fun () ->
          let fd = Socket.fd sock in
          ignore (Rx_queue.poll_and_consume rx fd 10 [||] ~pos:0 ~nb:1 : int option)));
  [%expect {| (raised (Invalid_argument "index out of bounds")) |}]
;;

let%expect_test "Rx_queue.consume yields data after Tx" =
  let umem_config = { Umem.Config.default with fill_size = 4; frame_size = 4096 } in
  let socket_config = { Socket.Config.default with rx_size = 4; tx_size = 4 } in
  test_with ~frame_cnt:8 ~umem_config ~socket_config (fun mem (_, fill, _) (s, rx, tx) ->
      let desc = Desc.{ addr = 0; len = 33; options = 0 } in
      set_pkt mem desc.addr;
      let filled = Fill_queue.produce fill [| 4096 |] ~pos:0 ~nb:1 in
      let sent = Tx_queue.produce tx [| desc |] ~pos:0 ~nb:1 in
      Socket.wakeup_kernel_with_sendto s;
      desc.addr <- 0;
      desc.len <- 0;
      desc.options <- 0;
      let consumed = Rx_queue.consume rx [| desc |] ~pos:0 ~nb:1 in
      Stdio.printf "%d %d %d %d %d" sent filled consumed desc.addr desc.len);
  [%expect {| 1 1 1 4096 33 |}]
;;

let%expect_test "Rx_queue.poll_and_consume yields data after Tx" =
  let umem_config = { Umem.Config.default with fill_size = 4; frame_size = 4096 } in
  let socket_config =
    { Socket.Config.default with
      rx_size = 4
    ; tx_size = 4
    ; xdp_flags = [ XDP_FLAGS_SKB_MODE ]
    }
  in
  test_with ~frame_cnt:8 ~umem_config ~socket_config (fun mem (_, fill, _) (s, rx, tx) ->
      let desc = Desc.{ addr = 0; len = 33; options = 0 } in
      set_pkt mem desc.addr;
      let fd = Socket.fd s in
      let filled = Fill_queue.produce fill [| 4096 |] ~pos:0 ~nb:1 in
      let sent = Tx_queue.produce tx [| desc |] ~pos:0 ~nb:1 in
      Socket.wakeup_kernel_with_sendto s;
      desc.addr <- 0;
      desc.len <- 0;
      desc.options <- 0;
      let ret = Rx_queue.poll_and_consume rx fd 1000 [| desc |] ~pos:0 ~nb:1 in
      let consumed = Option.value_exn ret in
      Stdio.printf "%d %d %d %d %d" sent filled consumed desc.addr desc.len);
  [%expect {| 1 1 1 4096 33 |}]
;;

(* Create socket - error on invalid parameters - error on OS error *)

(* Create Umem - error on invalid parameters - error on OS error - ring sizes must be
   powers of two - memory must be page aligned *)
