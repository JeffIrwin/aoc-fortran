
# This makefile builds the AOC static library
#
# Do not make this makefile on its own.  It is meant to be called from one of
# the daily makefiles, e.g. in the 2025/10 directory, where all the options like
# FC and FFLAGS are set:
#
#     cd 2025/10
#     make [debug|release]
#     ./main -h

# Source files. Order matters
SRC = \
	utils.F90 \
	blarg.F90 \
	map_i32.F90 \
	map_i64.F90 \
	map_str.F90 \
	map.F90 \
	args.F90

# Object files
OBJS = $(SRC:.F90=.o)

# Static library
LIB = aoc.a

#=======================================================================
# Build rules

all: debug

# Interdependencies within lib

args.o:  utils.F90
blarg.o: utils.F90

map.o: map_i32.F90
map.o: map_i64.F90
map.o: map_str.F90
map_i32.o: utils.F90
map_i64.o: utils.F90
map_str.o: utils.F90

#****************

debug:   $(LIB)
release: $(LIB)

.PHONY: all clean debug release

$(LIB): $(OBJS)
	$(AR) -crs $(LIB) $(OBJS)

%.o: %.F90
	$(FC) $(FFLAGS) -c $< -o $@

clean:
	rm -f $(LIB) $(OBJS)

