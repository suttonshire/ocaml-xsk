type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
(**  *)

module Desc : sig
  type t =
    { mutable addr : int
    ; mutable len : int
    ; mutable options : int
    }
end

exception Xsk_C_Failure of int

(* src/libbpf_c/libbpf/include/uapi/linux/if_xdp.h *)
module Umem_flag : sig
  type t = XDP_UMEM_UNALIGNED_CHUNK_FLAG
end

(* src/libbpf_c/libbpf/include/uapi/linux/if_link.h *)
module Xdp_flag : sig
  type t =
    | XDP_FLAGS_UPDATE_IF_NOEXIST
    | XDP_FLAGS_SKB_MODE
    | XDP_FLAGS_DRV_MODE
    | XDP_FLAGS_HW_MODE
    | XDP_FLAGS_REPLACE
end

(* src/libbpf_c/libbpf/src/xsk.h *)
module Libbpf_flag : sig
  type t = XSK_LIBBPF_FLAGS__INHIBIT_PROG_LOAD
end

(* src/libbpf_c/libbpf/include/uapi/linux/if_xdp.h *)
module Bind_flag : sig
  type t =
    | XDP_SHARED_UMEM
    | XDP_COPY
    | XDP_ZEROCOPY
    | XDP_USE_NEED_WAKEUP
end

module Fill_queue : sig
  type t

  (** [needs_wakeup t] is true if the kernel needs to be woken up to consume from [t]. To
      wake up the kernel to consume from the fill queue, [Xsk.poll] should be called.

      For more information see {{:
      https://github.com/torvalds/linux/blob/master/Documentation/networking/af_xdp.rst#xdp_use_need_wakeup-bind-flag
      } the kernel references } *)
  val needs_wakeup : t -> bool

  (** [produce t a ~pos ~nb] puts [nb] addresses from [a] into [t]. The addresses are read
      from [a] starting at [pos]. Returns the number of addresses produced to [t]. Raises
      if [pos + nb -1] > [Array.length a] *)
  val produce : t -> int array -> pos:int -> nb:int -> int

  (** [produce_and_wakeup_kernel t sock a ~pos ~nb] is the same as [produce t a ~pos ~nb]
      but the kernel will be woken up by calling sendto on [sock] if [needs_wakeup t] is
      true *)
  val produce_and_wakeup_kernel
    :  t
    -> Unix.file_descr
    -> int array
    -> pos:int
    -> nb:int
    -> int
end

module Comp_queue : sig
  type t

  (** [consume t a ~pos ~nb] consumes up to [~nb] addresses from [t] and puts them in [a]
      starting a index [~pos]. Returns the number of addresses consumed from [t]. Raises
      if [pos + nb -1] > [Array.length a] *)
  val consume : t -> int array -> pos:int -> nb:int -> int
end

module Tx_queue : sig
  type t

  (** [needs_wakeup t] is true if the kernel needs to be woken up to consume from [t]. To
      wake up the kernel to consume from the fill queue, [Socket.sendto] should be called
      on the socket associated with [t].

      For more information see {{:
      https://github.com/torvalds/linux/blob/master/Documentation/networking/af_xdp.rst#xdp_use_need_wakeup-bind-flag
      } the kernel references } *)
  val needs_wakeup : t -> bool

  (** [produce t a ~pos ~nb] puts [nb] descriptors from [a] into [t]. The descriptors are
      read from [a] starting at [pos]. Returns the number of addresses produced to [t].
      Raises if [pos + nb -1] > [Array.length a] *)
  val produce : t -> Desc.t array -> pos:int -> nb:int -> int

  (** [produce_and_wakeup_kernel t sock a ~pos ~nb] is the same as [produce t a ~pos ~nb]
      but the kernel will be woken up by calling sendto on [sock] if [needs_wakeup t] is
      true *)
  val produce_and_wakeup_kernel
    :  t
    -> Unix.file_descr
    -> Desc.t array
    -> pos:int
    -> nb:int
    -> int
end

module Rx_queue : sig
  type t

  (** [consume t a ~pos ~nb] consumes up to [~nb] descriptors from [t] and puts them in
      [a] starting a index [~pos]. Returns the number of descriptors consumed from [t].
      Raises if [pos + nb -1] > [Array.length a] *)
  val consume : t -> Desc.t array -> pos:int -> nb:int -> int

  (** [poll_and_consume t sock ms a ~pos ~nb] waits for upto [ms] milliseconds for [sock]
      to be readable. When [sock] becomes readable up to [~nb] descriptors are consumed
      from [t] and put in [a] starting a index [~pos]. Returns the number of descriptors
      consumed from [t] or [None] if [sock] does not become readable in [ms] millisecons.
      Raises if [pos + nb -1] > [Array.length a] *)
  val poll_and_consume
    :  t
    -> Unix.file_descr
    -> int
    -> Desc.t array
    -> pos:int
    -> nb:int
    -> int option
end

module Umem : sig
  type t

  module Config : sig
    type t =
      { fill_size : int
      ; comp_size : int
      ; frame_size : int
      ; frame_headroom : int
      ; flags : Umem_flag.t list
      }

    val default : t
  end

  val create : buffer -> int -> Config.t -> t * Fill_queue.t * Comp_queue.t
  val delete : t -> unit
  val fd : t -> Unix.file_descr
end

module Socket : sig
  type t

  module Config : sig
    type t =
      { rx_size : int
      ; tx_size : int
      ; libbpf_flags : Libbpf_flag.t list
      ; xdp_flags : Xdp_flag.t list
      ; bind_flags : Bind_flag.t list
      }

    val default : t
  end

  val create : string -> int -> Umem.t -> Config.t -> t * Rx_queue.t * Tx_queue.t
  val delete : t -> unit
  val fd : t -> Unix.file_descr
  val wakeup_kernel_with_sendto : t -> unit
end