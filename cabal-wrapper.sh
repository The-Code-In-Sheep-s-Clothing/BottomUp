#!/bin/bash

name=${0##*/}
cmd=${name##*-}
target=${name%-*}
 
fcommon="--builddir=dist/${target}"
fcompile=" --with-ghc=${target}-ghc"
fcompile+=" --with-ghc-pkg=${target}-ghc-pkg"
fcompile+=" --with-gcc=${target}-clang"
fcompile+=" --with-ld=${target}-ld"
fcompile+=" --hsc2hs-options=--cross-compile"
fconfig="--disable-shared --configure-option=--host=${target}"

case $1 in
  configure|install) flags="${fcommon} ${fcompile} ${fconfig}" ;;
  build)             flags="${fcommon} ${fcompile}" ;;
  list|info|update)  flags="" ;;
  "")                flags="" ;;
  *)                 flags=$fcommon ;;
esac

exec $cmd $flags "$@"