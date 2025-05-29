# syntax=docker/dockerfile:1.4

##
# This line ensures Docker pulls the correct multi-arch base image
# depending on the platform we specify during buildx.
##
FROM --platform=$TARGETPLATFORM haskell:9.6.6 AS builder

##
# Let's define architecture/platform arguments so we can do architecture-
# specific tasks (e.g., download the correct IPFS binary).
##
ARG TARGETARCH
ARG TARGETPLATFORM

RUN apt install curl ca-certificates
RUN install -d /usr/share/postgresql-common/pgdg
RUN curl -o /usr/share/postgresql-common/pgdg/apt.postgresql.org.asc --fail https://www.postgresql.org/media/keys/ACCC4CF8.asc
RUN sh -c 'echo "deb [signed-by=/usr/share/postgresql-common/pgdg/apt.postgresql.org.asc] https://apt.postgresql.org/pub/repos/apt bullseye-pgdg main" > /etc/apt/sources.list.d/pgdg.list'

RUN apt-get update -y && apt-get upgrade -y 
RUN apt-get install -y \
    automake \
    build-essential \
    pkg-config \
    libffi-dev \
    libgmp-dev \
    liblmdb-dev \
    libnuma-dev \
    libssl-dev \
    libsystemd-dev \
    libtinfo-dev \
    llvm-dev \
    zlib1g-dev \
    libpq-dev \
    lzma-dev \
    liblzma-dev \
    libtinfo-dev \
    libsystemd-dev \
    make \
    g++ \
    tmux \
    git \
    jq \
    wget \
    libncursesw5 \
    libtool \
    autoconf \
    libsqlite3-dev \
    m4 \
    ca-certificates \
    gcc \
    libc6-dev \
    && \
    apt-get clean
    

ENV PATH="/root/.cabal/bin:/root/.local/bin:$PATH"

RUN git clone https://github.com/IntersectMBO/libsodium && \
    cd libsodium && \
    curl -L -o build-aux/config.guess https://git.savannah.gnu.org/gitweb/?p=config.git;a=blob_plain;f=config.guess;hb=HEAD && \
    curl -L -o build-aux/config.sub https://git.savannah.gnu.org/gitweb/?p=config.git;a=blob_plain;f=config.sub;hb=HEAD && \
    chmod +x build-aux/config.guess build-aux/config.sub && \
    git fetch --all --recurse-submodules --tags && \
    git tag && \
    ./autogen.sh && \
    ./configure && \
    make && \
    make install && \
    cd .. && rm -rf libsodium
    
ENV LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
ENV PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"

RUN git clone --depth 1 --branch 'v0.3.2' https://github.com/bitcoin-core/secp256k1 && \
    cd secp256k1 && \
    ./autogen.sh && \
    ./configure --enable-module-schnorrsig --enable-experimental && \
    make && \
    make check && \
    make install

# BLST
COPY libblst.pc /usr/local/lib/pkgconfig/
RUN git clone https://github.com/supranational/blst && \
    cd blst && \
    git checkout ${BLST_REF} && \
    ./build.sh && \
    cp bindings/blst_aux.h bindings/blst.h bindings/blst.hpp  /usr/local/include/ && \
    cp libblst.a /usr/local/lib/ && \
    chmod u=rw,go=r /usr/local/lib/pkgconfig/libblst.pc \
      /usr/local/include/blst_aux.h /usr/local/include/blst.h /usr/local/include/blst.hpp \
      /usr/local/lib/libblst.a


WORKDIR /

# Install IPFS
RUN wget https://dist.ipfs.tech/kubo/v0.33.2/kubo_v0.33.2_linux-${TARGETARCH}.tar.gz && \
    tar -xvzf kubo_v0.33.2_linux-${TARGETARCH}.tar.gz 

RUN bash /kubo/install.sh

# Initialize IPFS
RUN ipfs init

RUN ipfs config Addresses.Gateway /ip4/0.0.0.0/tcp/8080
RUN ipfs config --json API.HTTPHeaders.Access-Control-Allow-Origin "[\"*\"]"
RUN ipfs config --json API.HTTPHeaders.Access-Control-Allow-Methods "[\"PUT\", \"GET\", \"POST\"]"
RUN ipfs config --json API.HTTPHeaders.Access-Control-Allow-Credentials "[\"true\"]"

# Start IPFS daemon
RUN ipfs daemon &

# Set environment variables for Plutus
ENV BLST_REF=master
ENV CABAL_BUILD_OPTIONS="--with-ghc=ghc-9.6.6 --allow-newer=plutus-tx-plugin:ghc-prim"

# Create build directory
WORKDIR /build

# Copy only the main cabal file and create a simplified cabal.project
COPY wine-project.cabal ./
COPY cabal.project ./

# Create cabal config directory and update
RUN mkdir -p /root/.config/cabal
RUN cabal update

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)

RUN cabal build --only-dependencies -j10 ${CABAL_BUILD_OPTIONS}

# Copy the rest of the source code
COPY . .

RUN update-alternatives --install /usr/bin/ld ld /usr/bin/ld.bfd 100

RUN cabal build all --ghc-options="-optl-Wl,--stub-group-size=0x3FFDFFE"  

# Add and Install Application Code
RUN cabal install server --ghc-options="-optl-Wl,--stub-group-size=0x3FFDFFE"  
