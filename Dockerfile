FROM ubuntu:20.04

USER root

RUN apt-get update -qq -y && \
    DEBIAN_FRONTEND="noninteractive" apt-get install -qq -y clang llvm libelf-dev linux-base zlib1g-dev \
    gcc-multilib ethtool apt-utils m4 pkg-config tcpdump iproute2 
    
RUN apt-get -y -qq install ocaml opam

RUN opam init --disable-sandboxing -y && opam update -y && \
    opam switch install 4.10.0+flambda && \
    opam switch 4.10.0+flambda && \
    opam init --disable-sandboxing -y && \
    eval $(opam env) && \
    opam upgrade -y
RUN opam install -y base dune ppx_jane base_bigstring ppx_cstruct cstruct expect_test_helpers_kernel
    
COPY . /root
RUN cd /root && eval $(opam env)
WORKDIR /root
ENTRYPOINT [ "test/docker_run.sh" ]