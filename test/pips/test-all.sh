#!/usr/bin/env bash

set -exu

# TODO: move inputs and screenshots to subdirs

debug_tests=()
debug_tests+=("easy-2025-12-28.txt")

debug_tests+=("medium-2025-12-27.txt")  # aka "test-input.txt"
debug_tests+=("medium-2025-12-28.txt")

debug_tests+=("hard-2025-12-28.txt")

release_tests=("${debug_tests[@]}")
release_tests+=("hard-2025-12-26.txt")
release_tests+=("hard-2025-12-27.txt")  # aka "input.txt", slow!

#********

ndebug=0
nrelease=0

rm main
./run.sh -t debug  # run test just to build
for test in "${debug_tests[@]}" ; do
	./main -a -i "$test"
	((ndebug++)) || true
done

rm main
./run.sh -t release  # run test just to build
for test in "${release_tests[@]}" ; do
	./main -a -i "$test"
	((nrelease++)) || true
done

set +x
echo "All ${ndebug} debug tests passed"
echo "All ${nrelease} release tests passed"
echo

