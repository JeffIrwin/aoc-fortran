
# This makefile builds a Fortran project

# Source files
SRC = \
	utils.F90 \
	blarg.F90 \
	args.F90

# Object files
OBJS = $(SRC:.F90=.o)

# Static library
LIB = aoc.a

#=======================================================================
# Build rules

all: debug

# Interdependencies within lib
args.o: utils.F90
blarg.o: utils.F90

debug:   $(LIB)
release: $(LIB)

$(LIB): $(OBJS)
	$(AR) -crs $(LIB) $(OBJS)

%.o: %.F90
	$(FC) $(FFLAGS) -c $< -o $@

clean:
	rm -f $(LIB) $(OBJS)

