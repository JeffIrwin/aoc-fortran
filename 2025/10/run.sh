#!/usr/bin/env bash

set -exu

make
time ./main $*

