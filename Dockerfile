
FROM rockylinux:9

RUN dnf update -y && dnf install -y \
	gfortran \
	which

WORKDIR /workdir/aoc-fortran

# bust cache
RUN echo 1

# The .dockerfile stops ADD from copying build output, e.g. .o files
ADD . .

ARG AOC_GPG_PASSPHRASE
#ENV AOC_GPG_PASSPHRASE=$AOC_GPG_PASSPHRASE

# The rest should be a bash script (run.sh), not directly in Dockerfile

#****************
WORKDIR /workdir/aoc-fortran/2025/10

RUN ../../decrypt-aoc.sh input.txt.gpg
RUN ../../decrypt-aoc.sh test-input.txt.gpg

# TODO: cover debug and release profiles. Do every day with debug and then every
# day with release to avoid extra cleaning and rebuilding
RUN make
RUN ./main --assert -t
RUN ./main --assert

#****************
WORKDIR /workdir/aoc-fortran/2025/11

RUN ../../decrypt-aoc.sh input.txt.gpg
RUN ../../decrypt-aoc.sh test-input.txt.gpg
RUN ../../decrypt-aoc.sh test-input2.txt.gpg

RUN make
RUN ./main --assert -1 -t
RUN ./main -2 -i test-input2.txt #--assert
RUN ./main --assert

#****************

