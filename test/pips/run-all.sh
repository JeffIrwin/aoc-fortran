#!/usr/bin/env bash

# Run all inputs for benchmarking, without correctness checks

set -exu

./run.sh -t release  # run test just to build
n=0
for f in inputs/*.json ; do
	time ./main --input "$f"
	((n++)) || true
done

set +x
echo "All ${n} JSON inputs ran"
echo

