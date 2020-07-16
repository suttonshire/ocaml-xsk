# ocaml-xsk

![CI](https://github.com/suttonshire/ocaml-xsk/workflows/CI/badge.svg?branch=master)

This is an OCaml binding to the XSK interface of libbpf. XSK is a simplified inteface to the linux specific AF_XDP sockets. The best introduction to AF_XDP sockets is from the kernel [documentation](https://github.com/torvalds/linux/blob/master/Documentation/networking/af_xdp.rst).

Testing shows that ocaml-xsk can transmit or receive packets at the line-rate of a 10Gb ethernet NIC.

[libbpf](https://github.com/libbpf/libbpf) is a submodule to this repo. We use the v0.0.8 tag. When you clone this repo remember to use a recursive clone:

```bash
git clone --recursive https://github.com/suttonshire/ocaml-xsk.git
```

libbpf depends on libelf and  zlib so you should install those on your system. e.g. on Ubuntu:

```bash
sudo apt-get install -y libelf-dev zlib1g-dev
```

## Running tests

The tests require root permissions in order to configure network interfaces. To avoid setting up a root OCaml environment and messing with network configs on development machines, a Dockerfile is provided to run tests in a container. You'll have to run the container with the privileged flag set. You can run the test like this:

```bash
sudo docker build . -t test && docker run --privileged test
```

AF_XDP is a fast moving interface under very active development by the kernel team. As such, many features supported in ocaml-xsk only work on the latest linux kernels. I've tested this interface on kernels > 5.4.

## Benchmarking

I don't have a local test setup with nice NICs so I've been testing this code on bare-metal cloud servers hosted by [Packet](https://www.packet.com/). The server config is:

- Xeon E 2278G 8-Core Processor @ 3.4Ghz
- Mellanox ConnectX-4 Ethernet NIC using the mlx5 driver

To run the benchmarks you have to make sure that traffic will go tothe correct NIC receive queue. You can configure that like so:

```bash
ethtool -N enp1s0f1 flow-type ether dst <rxdrop eth mac> src <udp_pkt_gen eth mac> action <rxdrop queue>
```

This command sends all traffic from the transmitting benchmark NIC to the receive queue on the receiving benchmark NIC. Running the benchmark tools should result in something like this:

```bash
root@ny5-c3:~/ocaml-xsk# ip link set dev enp1s0f1 xdp off; _build/default/bench/udp_pkt_gen.exe -d enp1s0f1 -dip 192.168.1.2:9999 -dmac 0c:42:a1:3d:85:19 -sip 192.168.1.1:9999 -smac 1c:34:da:5c:3d:15 -q 16 -c 105000000 -w -z
[udp_pkt_gen] 105000000 (pkts) 7.061761 (s) 14868813.408774 (pps)
```

```bash
root@ny5-c3:~/ocaml-xsk# ip link set dev enp1s0f1 xdp off; _build/default/bench/rxdrop.exe -d enp1s0f1 -q 16 -w -z -c 100000000
[rxdrop] max: 14875835.710268 (pps) min: 14827353.113507 (pps)
```
