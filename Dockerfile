
FROM rockylinux:9

ARG AOC_GPG_PASSPHRASE

RUN dnf update -y && dnf install -y \
	cmake \
	gfortran \
	which

WORKDIR /workdir/aoc-fortran

# The .dockerignore stops ADD from copying build output, e.g. .o files
ADD . .

#****************
# Run unit test
WORKDIR /workdir/aoc-fortran/test/unit

#RUN cmake -S . -B build && cmake --build build #--verbose
#RUN ./main
RUN ./run.sh  # debug by default
RUN ./run.sh Release

#****************
# The rest should be a bash script (run.sh), not directly in Dockerfile
#
# TODO: cover debug and release profiles. Do every day with debug and then every
# day with release to avoid extra cleaning and rebuilding

#****************
WORKDIR /workdir/aoc-fortran/2025/7

RUN ../../scripts/decrypt-aoc.sh input.txt.gpg
RUN ../../scripts/decrypt-aoc.sh test-input.txt.gpg

RUN cmake -S . -B build && cmake --build build #--verbose
RUN ./main --assert --test
RUN ./main --assert

#****************
WORKDIR /workdir/aoc-fortran/2025/8

RUN ../../scripts/decrypt-aoc.sh input.txt.gpg
RUN ../../scripts/decrypt-aoc.sh test-input.txt.gpg

RUN cmake -S . -B build && cmake --build build #--verbose
RUN ./main --assert --test
RUN ./main --assert

#****************
WORKDIR /workdir/aoc-fortran/2025/10

# Should be able to just run decrypt-everything.sh to do this for all dirs

RUN ../../scripts/decrypt-aoc.sh input.txt.gpg
RUN ../../scripts/decrypt-aoc.sh test-input.txt.gpg

RUN cmake -S . -B build && cmake --build build #--verbose
RUN ./main --assert -t
RUN ./main --assert

#****************
WORKDIR /workdir/aoc-fortran/2025/11

RUN ../../scripts/decrypt-aoc.sh input.txt.gpg
RUN ../../scripts/decrypt-aoc.sh test-input.txt.gpg
RUN ../../scripts/decrypt-aoc.sh test-input2.txt.gpg

RUN cmake -S . -B build && cmake --build build #--verbose
RUN ./main --assert -1 -t
RUN ./main -2 -i test-input2.txt #--assert
RUN ./main --assert

#****************
# Run NYT pips test. This doesn't inherently belong in this repo except I have a
# nice library and infrastructure for running puzzle solving programs in Fortran
WORKDIR /workdir/aoc-fortran/test/pips

RUN ./test-all.sh

