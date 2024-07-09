#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>

#include "xsk.h"
#include <errno.h>
#include <poll.h>
#include <stdio.h>
#include <sys/socket.h>

enum desc_fields { addr, len, options };

enum socket_config_fields {
  rx_size,
  tx_size,
  libbpf_flags,
  xdp_flags,
  bind_flags
};

enum umem_config_fields {
  fill_size,
  comp_size,
  frame_size,
  frame_headroom,
  flags
};

#define Ring_cons_ptr_val(vr) ((struct xsk_ring_cons *)Data_custom_val(vr))
#define Ring_prod_ptr_val(vr) ((struct xsk_ring_prod *)Data_custom_val(vr))
#define Umem_ptr_val(vumem) (*((struct xsk_umem **)Data_custom_val(vumem)))
#define Socket_ptr_val(vsock) (*((struct xsk_socket **)Data_custom_val(vsock)))

void raise_errno(int err) {
  caml_raise_with_arg(*caml_named_value("xsk exception"), Val_int(err));
}

static struct custom_operations ring_custom_ops = {
    .identifier = "ring",
    .finalize = custom_finalize_default,
    .compare = custom_compare_default,
    .hash = custom_hash_default,
    .serialize = custom_serialize_default,
    .deserialize = custom_deserialize_default,
    .compare_ext = custom_compare_ext_default};

static struct custom_operations xsk_umem_custom_ops = {
    .identifier = "xsk_umem",
    .finalize = custom_finalize_default,
    .compare = custom_compare_default,
    .hash = custom_hash_default,
    .serialize = custom_serialize_default,
    .deserialize = custom_deserialize_default,
    .compare_ext = custom_compare_ext_default};

static struct custom_operations xsk_socket_custom_ops = {
    .identifier = "xsk_socket",
    .finalize = custom_finalize_default,
    .compare = custom_compare_default,
    .hash = custom_hash_default,
    .serialize = custom_serialize_default,
    .deserialize = custom_deserialize_default,
    .compare_ext = custom_compare_ext_default};

CAMLprim intnat ring_cons_nb_avail_nat(value vr, intnat nb) {
  struct xsk_ring_cons *r = Ring_cons_ptr_val(vr);
  return xsk_cons_nb_avail(r, nb);
}

CAMLprim value ring_cons_nb_avail(value vr, value vnb) {
  return Val_int(ring_cons_nb_avail_nat(vr, Int_val(vnb)));
}

CAMLprim value ring_prod_needs_wakeup(const value vr) {
  const struct xsk_ring_prod *r = Ring_prod_ptr_val(vr);
  return Val_bool(xsk_ring_prod__needs_wakeup(r));
}

CAMLprim intnat ring_prod_nb_free_nat(value vr, intnat nb) {
  struct xsk_ring_prod *r = Ring_prod_ptr_val(vr);
  return xsk_prod_nb_free(r, nb);
}

CAMLprim value ring_prod_nb_free(value vr, value vnb) {
  return Val_int(ring_prod_nb_free_nat(vr, Int_val(vnb)));
}

CAMLprim value umem_create(value vmem, value vsize, value vconfig) {
  struct xsk_umem *umem;
  struct xsk_ring_prod fill = {0};
  struct xsk_ring_cons comp = {0};
  struct xsk_umem_config config = {0};
  void *umem_area;
  unsigned long long size;
  int err;

  CAMLparam3(vmem, vsize, vconfig);
  CAMLlocal4(vfill, vcomp, vumem, vret);
  size = Int_val(vsize);
  umem_area = Caml_ba_data_val(vmem);
  config.fill_size = Int_val(Field(vconfig, fill_size));
  config.comp_size = Int_val(Field(vconfig, comp_size));
  config.frame_size = Int_val(Field(vconfig, frame_size));
  config.frame_headroom = Int_val(Field(vconfig, frame_headroom));
  config.flags = Int_val(Field(vconfig, flags));

  caml_release_runtime_system();
  err = xsk_umem__create(&umem, umem_area, size, &fill, &comp, &config);
  caml_acquire_runtime_system();
  if (err < 0) {
    raise_errno(-err);
  }

  vfill = caml_alloc_custom(&ring_custom_ops, sizeof(fill), 0, 1);
  *Ring_prod_ptr_val(vfill) = fill;

  vcomp = caml_alloc_custom(&ring_custom_ops, sizeof(comp), 0, 1);
  *Ring_cons_ptr_val(vcomp) = comp;

  vumem = caml_alloc_custom_mem(&xsk_umem_custom_ops, sizeof(umem), 0);
  Umem_ptr_val(vumem) = umem;

  vret = caml_alloc_tuple(3);
  Store_field(vret, 0, vumem);
  Store_field(vret, 1, vfill);
  Store_field(vret, 2, vcomp);

  CAMLreturn(vret);
}

CAMLprim value umem_delete(value vumem) {
  struct xsk_umem *umem;
  CAMLparam1(vumem);

  umem = Umem_ptr_val(vumem);
  xsk_umem__delete(umem);

  CAMLreturn(Val_unit);
}

CAMLprim value socket_create(value vifname, value vqueue_id, value vumem,
                             value vconfig) {
  struct xsk_socket *xsk;
  const char *ifname;
  unsigned int queue_id;
  struct xsk_umem *umem;
  struct xsk_ring_prod tx = {0};
  struct xsk_ring_cons rx = {0};
  struct xsk_socket_config config = {0};
  int err;
  CAMLparam4(vifname, vqueue_id, vumem, vconfig);
  CAMLlocal4(vsock, vrx, vtx, vret);

  ifname = String_val(vifname);
  queue_id = Int_val(vqueue_id);
  umem = Umem_ptr_val(vumem);
  config.rx_size = Int_val(Field(vconfig, rx_size));
  config.tx_size = Int_val(Field(vconfig, tx_size));
  config.libbpf_flags = Int_val(Field(vconfig, libbpf_flags));
  config.xdp_flags = Int_val(Field(vconfig, xdp_flags));
  config.bind_flags = Int_val(Field(vconfig, bind_flags));

  caml_release_runtime_system();
  err = xsk_socket__create(&xsk, ifname, queue_id, umem, &rx, &tx, &config);
  caml_acquire_runtime_system();
  if (err < 0) {
    raise_errno(-err);
  }

  vrx = caml_alloc_custom(&ring_custom_ops, sizeof(rx), 0, 1);
  *Ring_cons_ptr_val(vrx) = rx;

  vtx = caml_alloc_custom(&ring_custom_ops, sizeof(tx), 0, 1);
  *Ring_prod_ptr_val(vtx) = tx;

  vsock = caml_alloc_custom_mem(&xsk_socket_custom_ops, sizeof(xsk), 0);
  Socket_ptr_val(vsock) = xsk;

  vret = caml_alloc_tuple(3);
  Store_field(vret, 0, vsock);
  Store_field(vret, 1, vrx);
  Store_field(vret, 2, vtx);

  CAMLreturn(vret);
}

CAMLprim value socket_delete(value vsocket) {
  struct xsk_socket *sock;
  CAMLparam1(vsocket);

  sock = Socket_ptr_val(vsocket);
  xsk_socket__delete(sock);

  CAMLreturn(Val_unit);
}

CAMLprim value socket_fd(value vsock) {
  struct xsk_socket *sock;
  int fd;
  CAMLparam1(vsock);

  sock = Socket_ptr_val(vsock);
  fd = xsk_socket__fd(sock);
  if (fd < 0) {
    raise_errno(-fd);
  }

  CAMLreturn(Val_int(fd));
}

CAMLprim value socket_sendto_nat(intnat sock) {
  int ret;

  ret = sendto(sock, NULL, 0, MSG_DONTWAIT, NULL, 0);
  if (ret >= 0)
    return Val_unit;

  if (errno != ENOBUFS && errno != EAGAIN && errno != EBUSY &&
      errno != ENETDOWN) {
    raise_errno(errno);
  }

  return Val_unit;
}

CAMLprim value socket_sendto(value vsock) {
  return socket_sendto_nat(Int_val(vsock));
}

CAMLprim value socket_pollin_nat(intnat sock, intnat timeout) {
  struct pollfd fd = {0};
  int ret;

  fd.fd = sock;
  fd.events = POLLIN;
  ret = poll(&fd, 1, timeout);
  if (ret < 0) {
    if (errno != EINTR) {
      raise_errno(errno);
    } else {
      return Val_false;
    }
  }

  if (ret == 0)
    return Val_false;

  return Val_true;
}

CAMLprim value socket_pollin(value vsock, value vtimeout) {
  return socket_pollin_nat(Int_val(vsock), Int_val(vtimeout));
}

CAMLprim value socket_pollout_nat(intnat sock, intnat timeout) {
  struct pollfd fd = {0};
  int ret;

  fd.fd = sock;
  fd.events = POLLOUT;
  ret = poll(&fd, 1, timeout);
  if (ret < 0) {
    if (errno != EINTR) {
      raise_errno(errno);
    } else {
      return Val_false;
    }
  }

  if (ret == 0)
    return Val_false;

  return Val_true;
}

CAMLprim value socket_pollout(value vsock, value vtimeout) {
  return socket_pollout_nat(Int_val(vsock), Int_val(vtimeout));
}

CAMLprim intnat comp_queue_cons_nat(value vr, value varr, intnat pos,
                                    intnat nb) {
  struct xsk_ring_cons *r;
  unsigned int idx;
  size_t cnt;

  r = Ring_cons_ptr_val(vr);
  cnt = xsk_ring_cons__peek(r, nb, &idx);
  for (size_t i = 0; i < cnt; i++) {
    Field(varr, pos++) = Val_int(*xsk_ring_cons__comp_addr(r, idx++));
  }

  if (cnt > 0)
    xsk_ring_cons__release(r, cnt);

  return cnt;
}

CAMLprim value comp_queue_cons(value vr, value varr, value vpos, value vnb) {
  return Val_int(comp_queue_cons_nat(vr, varr, Int_val(vpos), Int_val(vnb)));
}

CAMLprim intnat fill_queue_produce_nat(value vr, value varr, intnat pos,
                                       intnat nb) {
  struct xsk_ring_prod *r;
  unsigned int idx;
  size_t cnt;

  r = Ring_prod_ptr_val(vr);
  cnt = xsk_ring_prod__reserve(r, nb, &idx);
  for (size_t i = 0; i < cnt; i++, idx++) {
    *xsk_ring_prod__fill_addr(r, idx) = Int_val(Field(varr, pos + i));
  }
  if (cnt > 0)
    xsk_ring_prod__submit(r, cnt);

  return cnt;
}

CAMLprim value fill_queue_produce(value vr, value varr, value vpos, value vnb) {
  return Val_int(fill_queue_produce_nat(vr, varr, Int_val(vpos), Int_val(vnb)));
}

CAMLprim intnat fill_queue_produce_and_wakeup_nat(value vr, intnat sock,
                                                  intnat timeout, value varr,
                                                  intnat pos, intnat nb) {
  struct xsk_ring_prod *r;
  unsigned int idx;
  size_t cnt;

  r = Ring_prod_ptr_val(vr);
  cnt = xsk_ring_prod__reserve(r, nb, &idx);

  for (size_t i = 0; i < cnt; i++, idx++) {
    *xsk_ring_prod__fill_addr(r, idx) = Int_val(Field(varr, pos + i));
  }

  if (cnt > 0) {
    xsk_ring_prod__submit(r, cnt);
    if (xsk_ring_prod__needs_wakeup(r)) {
      (void)socket_pollin_nat(sock, timeout);
    }
  }

  return cnt;
}

CAMLprim value fill_queue_produce_and_wakeup(value *argv, int argc) {
  if (argc != 6) {
    raise_errno(EINVAL);
  }
  return Val_int(fill_queue_produce_and_wakeup_nat(
      argv[0], Int_val(argv[1]), Int_val(argv[2]), argv[3], Int_val(argv[4]),
      Int_val(argv[5])));
}

CAMLprim intnat tx_queue_produce_nat(value vr, value varr, intnat pos,
                                     intnat nb) {
  struct xsk_ring_prod *r;
  unsigned int idx;
  size_t cnt;
  struct xdp_desc *desc;

  r = Ring_prod_ptr_val(vr);
  cnt = xsk_ring_prod__reserve(r, nb, &idx);
  for (size_t i = 0; i < cnt; i++) {
    desc = xsk_ring_prod__tx_desc(r, idx++);
    value v = Field(varr, pos++);
    desc->addr = Int_val(Field(v, addr));
    desc->len = Int_val(Field(v, len));
    desc->options = Int_val(Field(v, options));
  }
  if (cnt > 0)
    xsk_ring_prod__submit(r, cnt);

  return cnt;
}

CAMLprim value tx_queue_produce(value vr, value varr, value vpos, value vnb) {
  return Val_int(tx_queue_produce_nat(vr, varr, Int_val(vpos), Int_val(vnb)));
}

CAMLprim intnat rx_queue_cons_nat(value vr, value varr, intnat pos, intnat nb) {
  struct xsk_ring_cons *r;
  unsigned int idx;
  size_t cnt;
  const struct xdp_desc *desc;

  r = Ring_cons_ptr_val(vr);
  cnt = xsk_ring_cons__peek(r, nb, &idx);
  for (size_t i = 0; i < cnt; i++) {
    desc = xsk_ring_cons__rx_desc(r, idx++);
    value vdesc = Field(varr, pos++);
    Field(vdesc, addr) = Val_int(desc->addr);
    Field(vdesc, len) = Val_int(desc->len);
    Field(vdesc, options) = Val_int(desc->options);
  }
  if (cnt > 0)
    xsk_ring_cons__release(r, cnt);

  return cnt;
}

CAMLprim value rx_queue_cons(value vr, value varr, value vpos, value vnb) {
  return Val_int(rx_queue_cons_nat(vr, varr, Int_val(vpos), Int_val(vnb)));
}

CAMLprim intnat rx_queue_poll_cons_nat(value vr, intnat sock, intnat timeout,
                                       value varr, intnat pos, intnat nb) {
  if (socket_pollin_nat(sock, timeout) == Val_false)
    return -1;

  return rx_queue_cons_nat(vr, varr, pos, nb);
}

CAMLprim value rx_queue_poll_cons(value *argv, int argc) {
  if (argc != 6) {
    raise_errno(EINVAL);
  }
  return rx_queue_poll_cons_nat(argv[0], Int_val(argv[1]), Int_val(argv[2]),
                                argv[3], Int_val(argv[4]), Int_val(argv[5]));
}
