ECHOIF=echo
ECHOHWADDR=06:06:06:06:06:06
export XSK_TEST_SCRIPT_DIR=$1
ip tuntap add name $ECHOIF mode tap
ip link set dev $ECHOIF address $ECHOHWADDR
ip link set dev $ECHOIF xdpgeneric obj \
   $XSK_TEST_SCRIPT_DIR/xdp/xdp_prog_kern.o sec xdp_echo
ip link set dev $ECHOIF up

dune runtest

ip link del $ECHOIF
