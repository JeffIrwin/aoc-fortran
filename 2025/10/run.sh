#!/usr/bin/env bash

set -exu

mkdir -p build

# Debug
gfortran -o main main.f90 -fbounds-check -Wall -Wextra -Wno-tabs -fbacktrace -g -J./build/

## Release
#gfortran -o main main.f90 -O3 -Wall -Wextra -Wno-tabs

## cp for nvim linting nonsense
#cp *.mod build

time ./main

