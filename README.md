# ocaml-xsk

![CI](https://github.com/suttonshire/ocaml-xsk/workflows/CI/badge.svg?branch=master)

This is an OCaml to the XSK interface of libbpf. XSK is a simplified inteface to the linux specific AF_XDP sockets. The best introduction to AF_XDP sockets is from the kernel [documentation](https://github.com/torvalds/linux/blob/master/Documentation/networking/af_xdp.rst).

[libbpf](https://github.com/libbpf/libbpf) is a submodule to this repo. We use the v0.0.8 tag. libbpf depends on libelf and  zlib so you should install those on your system. e.g. on Ubuntu:

```bash
sudo apt-get install -y libelf-dev zlib1g-dev
```

## Running tests

The tests require root permissions in order to configure network interfaces. To avoid setting up a root OCaml environment and messing with network configs on development machines, a Dockerfile is provided to run tests in a container. You'll have to run the container with the privileged flag set. You can run the test like this:

```bash
sudo docker build . -t test && docker run --privileged test
```

AF_XDP is a fast moving interface under very active development by the kernel team. As such, many features supported in ocaml-xsk only work on the latest linux kernels. We've tested this interface on kernels > 5.4.
