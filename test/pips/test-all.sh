#!/usr/bin/env bash

# Run some tests with the "-a" arg to assert that their results match the
# expected output

set -exu

debug_tests=()
debug_tests+=("easy-2025-12-28.txt")
debug_tests+=("easy-2025-12-30.txt")

debug_tests+=("medium-2025-12-27.txt")  # aka "test-input.txt"
debug_tests+=("medium-2025-12-28.txt")
debug_tests+=("medium-2025-12-30.txt")

debug_tests+=("hard-2025-12-28.txt")

release_tests=("${debug_tests[@]}")
release_tests+=("hard-2025-12-26.txt")
release_tests+=("hard-2025-12-27.txt")  # aka "input.txt"
release_tests+=("hard-2025-12-30.txt")

release_tests+=("2025-12-24.json")

#********

ndebug=0
nrelease=0

./run.sh -t debug  # run test just to build
for test in "${debug_tests[@]}" ; do
	time ./main -a -i inputs/"$test"
	((ndebug++)) || true
done

./run.sh -t release  # run test just to build
for test in "${release_tests[@]}" ; do
	time ./main -a -i inputs/"$test"
	((nrelease++)) || true
done

set +x
echo "All ${ndebug} debug tests passed"
echo "All ${nrelease} release tests passed"
echo

