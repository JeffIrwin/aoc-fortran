
# This makefile builds a Fortran project

# Source files
SRC = \
	utils.F90 \
	args.F90

# Object files
OBJ = $(SRC:.F90=.o)

# Static library
LIB = aoc.a

# Build rules
all: $(LIB)

args.o: utils.F90

$(LIB): $(OBJ)
	$(AR) -crs $@ $^

%.o: %.F90
	mkdir -p $(BUILD_DIR)
	$(FC) $(FFLAGS) -c $< -o $@

clean:
	rm -f $(OBJ) $(LIB)

