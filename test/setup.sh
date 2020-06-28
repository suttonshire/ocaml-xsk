#!/bin/bash

TESTIF=$1
TESTHWADDR=$2
ECHOIF=$3
ECHOHWADDR=$4
SCRIPTDIR=$5
ip link add dev $TESTIF type veth peer name $ECHOIF
ip link set $TESTIF up
ip link set $ECHOIF up
ip link set dev $TESTIF address $TESTHWADDR
ip link set dev $ECHOIF address $ECHOHWADDR
ip link set dev $ECHOIF xdpgeneric obj \
   $SCRIPTDIR/xdp/xdp_prog_kern.o sec xdp_echo
