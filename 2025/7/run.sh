#!/usr/bin/env bash

set -exu

config=Debug
for arg in "$@"; do
	echo "arg = $arg"
	if [[ "${arg,,}" == "release" ]]; then
		config=Release
	fi
done

verbose=""
#verbose=--verbose

cmake -DCMAKE_BUILD_TYPE=${config} -S . -B build/${config}
cmake --build build/${config} ${verbose} --config ${config}

time ./main "$@"

