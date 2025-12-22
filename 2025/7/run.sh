#!/usr/bin/env bash

set -exu

# TODO: get config from cmd arg
config=Debug
config=Release

verbose=""
#verbose=--verbose

cmake -DCMAKE_BUILD_TYPE=${config} -S . -B build/${config}
cmake --build build/${config} ${verbose} --config ${config}

time ./main "$@"

