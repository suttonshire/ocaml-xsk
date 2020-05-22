#!/bin/bash

ECHOIF=echo
ECHOHWADDR=06:06:06:06:06:06
ip tuntap add name $ECHOIF mode tap
ip link set dev $ECHOIF address $ECHOHWADDR
ip link set dev $ECHOIF xdpgeneric obj xdp/xdp_prog_kern.o sec xdp_echo
ip link set dev $ECHOIF up

dune runtest

ip link del $ECHOIF
