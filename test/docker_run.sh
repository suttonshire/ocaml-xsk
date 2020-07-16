#! /bin/bash

export XSK_TEST_DEPS_DIR=$PWD/test
export XSK_TEST_INTF_NAME=test
export XSK_TEST_INTF_MAC=04:04:04:04:04:04
export XSK_ECHO_INTF_NAME=echo
export XSK_ECHO_INTF_MAC=06:06:06:06:06:06

eval $(opam env)
dune runtest --profile=test
