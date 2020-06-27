#!/bin/bash

TESTIF=$1
TESTHWADDR=$2
sudo ip tuntap add name $TESTIF mode tap
sudo ip link set dev $TESTIF address $TESTHWADDR
sudo ip link set dev $TESTIF up