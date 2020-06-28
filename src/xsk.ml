type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module Desc = struct
  type t =
    { mutable addr : int
    ; mutable len : int
    ; mutable options : int
    }
end

exception Xsk_C_Failure of int

let _ = Callback.register_exception "xsk exception" (Xsk_C_Failure 0)

(* src/libbpf_c/libbpf/include/uapi/linux/if_xdp.h *)
module Umem_flag = struct
  type t = XDP_UMEM_UNALIGNED_CHUNK_FLAG

  let to_int = function
    | XDP_UMEM_UNALIGNED_CHUNK_FLAG -> 1 lsl 0
  ;;

  let to_bitfield l = List.fold_left (fun bitfield flag -> bitfield lor to_int flag) 0 l
end

(* src/libbpf_c/libbpf/include/uapi/linux/if_link.h *)
module Xdp_flag = struct
  type t =
    | XDP_FLAGS_UPDATE_IF_NOEXIST
    | XDP_FLAGS_SKB_MODE
    | XDP_FLAGS_DRV_MODE
    | XDP_FLAGS_HW_MODE
    | XDP_FLAGS_REPLACE

  let to_int = function
    | XDP_FLAGS_UPDATE_IF_NOEXIST -> 1 lsl 0
    | XDP_FLAGS_SKB_MODE -> 1 lsl 1
    | XDP_FLAGS_DRV_MODE -> 1 lsl 2
    | XDP_FLAGS_HW_MODE -> 1 lsl 3
    | XDP_FLAGS_REPLACE -> 1 lsl 4
  ;;

  let to_bitfield l = List.fold_left (fun bitfield flag -> bitfield lor to_int flag) 0 l
end

(* src/libbpf_c/libbpf/src/xsk.h *)
module Libbpf_flag = struct
  type t = XSK_LIBBPF_FLAGS__INHIBIT_PROG_LOAD

  let to_int = function
    | XSK_LIBBPF_FLAGS__INHIBIT_PROG_LOAD -> 1 lsl 0
  ;;

  let to_bitfield l = List.fold_left (fun bitfield flag -> bitfield lor to_int flag) 0 l
end

(* src/libbpf_c/libbpf/include/uapi/linux/if_xdp.h *)
module Bind_flag = struct
  type t =
    | XDP_SHARED_UMEM
    | XDP_COPY
    | XDP_ZEROCOPY
    | XDP_USE_NEED_WAKEUP

  let to_int = function
    | XDP_SHARED_UMEM -> 1 lsl 0
    | XDP_COPY -> 1 lsl 1
    | XDP_ZEROCOPY -> 1 lsl 2
    | XDP_USE_NEED_WAKEUP -> 1 lsl 3
  ;;

  let to_bitfield l = List.fold_left (fun bitfield flag -> bitfield lor to_int flag) 0 l
end

type ring_cons
type ring_prod
type umem
type socket

external needs_wakeup_stub : ring_prod -> bool = "ring_prod_needs_wakeup" [@@noalloc]

external socket_sendto_stub
  :  (int[@untagged])
  -> unit
  = "socket_sendto" "socket_sendto_nat"
  [@@noalloc]

module Comp_queue = struct
  type t = ring_cons

  let create ring = ring

  external consume_stub
    :  ring_cons
    -> int array
    -> (int[@untagged])
    -> (int[@untagged])
    -> (int[@untagged])
    = "comp_queue_cons" "comp_queue_cons_nat"
    [@@noalloc]

  let consume t arr ~pos ~nb =
    ignore (arr.(pos + nb - 1) : int);
    consume_stub t arr pos nb
  ;;
end

module Fill_queue = struct
  type t = ring_prod

  let create ring = ring
  let needs_wakeup t = needs_wakeup_stub t

  external produce_stub
    :  ring_prod
    -> int array
    -> (int[@untagged])
    -> (int[@untagged])
    -> (int[@untagged])
    = "fill_queue_produce" "fill_queue_produce_nat"
    [@@noalloc]

  external produce_and_wakeup_stub
    :  ring_prod
    -> (int[@untagged])
    -> (int[@untagged])
    -> int array
    -> (int[@untagged])
    -> (int[@untagged])
    -> (int[@untagged])
    = "fill_queue_produce_and_wakeup" "fill_queue_produce_and_wakeup_nat"
    [@@noalloc]

  let produce t arr ~pos ~nb =
    ignore (arr.(pos + nb - 1) : int);
    produce_stub t arr pos nb
  ;;

  let produce_and_wakeup_kernel t (fd : Unix.file_descr) arr ~pos ~nb =
    ignore (arr.(pos + nb - 1) : int);
    produce_and_wakeup_stub t (Obj.magic fd) 1000 arr pos nb
  ;;
end

module Tx_queue = struct
  type t = ring_prod

  let create ring = ring
  let needs_wakeup t = needs_wakeup_stub t

  external produce_stub
    :  ring_prod
    -> Desc.t array
    -> (int[@untagged])
    -> (int[@untagged])
    -> (int[@untagged])
    = "tx_queue_produce" "tx_queue_produce_nat"
    [@@noalloc]

  let produce t arr ~pos ~nb =
    ignore (arr.(pos + nb - 1) : Desc.t);
    produce_stub t arr pos nb
  ;;

  let produce_and_wakeup_kernel t (fd : Unix.file_descr) arr ~pos ~nb =
    ignore (arr.(pos + nb - 1) : Desc.t);
    let ret = produce_stub t arr pos nb in
    if ret > 0 && needs_wakeup t then socket_sendto_stub (Obj.magic fd);
    ret
  ;;
end

module Rx_queue = struct
  type t = ring_cons

  let create ring = ring

  external consume_stub
    :  t
    -> Desc.t array
    -> (int[@untagged])
    -> (int[@untagged])
    -> (int[@untagged])
    = "rx_queue_cons" "rx_queue_cons_nat"
    [@@noalloc]

  external poll_and_consume_stub
    :  t
    -> (int[@untagged])
    -> (int[@untagged])
    -> Desc.t array
    -> (int[@untagged])
    -> (int[@untagged])
    -> (int[@untagged])
    = "rx_queue_poll_cons" "rx_queue_poll_cons_nat"
    [@@noalloc]

  let consume t arr ~pos ~nb =
    ignore (arr.(pos + nb - 1) : Desc.t);
    consume_stub t arr pos nb
  ;;

  let poll_and_consume t (fd : Unix.file_descr) timeout arr ~pos ~nb =
    ignore (arr.(pos + nb - 1) : Desc.t);
    let ret = poll_and_consume_stub t (Obj.magic fd) timeout arr pos nb in
    if ret = 0 then None else Some ret
  ;;
end

module Umem = struct
  type t =
    { umem : umem
    ; fill : ring_prod
    ; comp : ring_cons
    }

  module Config = struct
    type t =
      { fill_size : int
      ; comp_size : int
      ; frame_size : int
      ; frame_headroom : int
      ; flags : Umem_flag.t list
      }

    type t_internal =
      { fill_size : int
      ; comp_size : int
      ; frame_size : int
      ; frame_headroom : int
      ; flags_bitfield : int
      }

    let t_internal_of_t t =
      let flags_bitfield = Umem_flag.to_bitfield t.flags in
      { fill_size = t.fill_size
      ; comp_size = t.comp_size
      ; frame_size = t.frame_size
      ; frame_headroom = t.frame_headroom
      ; flags_bitfield
      }
    ;;

    let default_fill_queue_num_desc = 2048
    let default_comp_queue_num_desc = 2048
    let default_frame_size = 4096
    let default_frame_headroom = 0
    let default_flags = []

    let default =
      { fill_size = default_fill_queue_num_desc
      ; comp_size = default_comp_queue_num_desc
      ; frame_size = default_frame_size
      ; frame_headroom = default_frame_headroom
      ; flags = default_flags
      }
    ;;
  end

  external create_stub
    :  buffer
    -> int
    -> Config.t_internal
    -> umem * ring_prod * ring_cons
    = "umem_create"

  let create mem size config =
    let config = Config.t_internal_of_t config in
    let umem, fill, comp = create_stub mem size config in
    { umem; fill; comp }, Fill_queue.create fill, Comp_queue.create comp
  ;;

  external delete_stub : umem -> unit = "umem_delete"

  let delete t = delete_stub t.umem

  external fd_stub : umem -> Unix.file_descr = "umem_fd"

  let fd t = fd_stub t.umem
end

module Socket = struct
  type t =
    { sock : socket
    ; rx : ring_cons
    ; tx : ring_prod
    }

  module Config = struct
    type t =
      { rx_size : int
      ; tx_size : int
      ; libbpf_flags : Libbpf_flag.t list
      ; xdp_flags : Xdp_flag.t list
      ; bind_flags : Bind_flag.t list
      }

    type t_internal =
      { rx_size : int
      ; tx_size : int
      ; libbpf_flags_bitfield : int
      ; xdp_flags_bitfield : int
      ; bind_flags_bitfield : int
      }

    let t_internal_of_t t =
      let libbpf_flags_bitfield = Libbpf_flag.to_bitfield t.libbpf_flags in
      let xdp_flags_bitfield = Xdp_flag.to_bitfield t.xdp_flags in
      let bind_flags_bitfield = Bind_flag.to_bitfield t.bind_flags in
      { tx_size = t.tx_size
      ; rx_size = t.rx_size
      ; libbpf_flags_bitfield
      ; xdp_flags_bitfield
      ; bind_flags_bitfield
      }
    ;;

    let default_rx_queue_num_desc = 2048
    let default_tx_queue_num_desc = 2048
    let default_libbpf_flags = []
    let default_xdp_flags = []
    let default_bind_flags = []

    let default =
      { rx_size = default_rx_queue_num_desc
      ; tx_size = default_tx_queue_num_desc
      ; libbpf_flags = default_libbpf_flags
      ; xdp_flags = default_xdp_flags
      ; bind_flags = default_bind_flags
      }
    ;;
  end

  external create_stub
    :  string
    -> int
    -> umem
    -> Config.t_internal
    -> socket * ring_cons * ring_prod
    = "socket_create"

  let create ifname queue_id umem config =
    let config = Config.t_internal_of_t config in
    let sock, rx, tx = create_stub ifname queue_id umem.Umem.umem config in
    { sock; rx; tx }, Rx_queue.create rx, Tx_queue.create tx
  ;;

  external delete_stub : socket -> unit = "socket_delete"

  let delete t = delete_stub t.sock

  external fd_stub : socket -> Unix.file_descr = "socket_fd"

  let fd t = fd_stub t.sock
  let wakeup_kernel_with_sendto t = socket_sendto_stub (Obj.magic (fd t))
end
