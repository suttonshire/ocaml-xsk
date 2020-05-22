#!/bin/bash

TESTIF=test$1
TESTHWADDR=04:04:04:04:04:04
ip tuntap add name $TESTIF mode tap
ip link set dev $TESTIF address $TESTHWADDR
ip link set dev $TESTIF up
