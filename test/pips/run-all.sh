#!/usr/bin/env bash

# Run all inputs for benchmarking, without correctness checks
#
# But, if the solver finishes and provides *any* solution without panicking, it
# *should* be correct

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

