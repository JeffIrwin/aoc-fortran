
# This makefile builds a Fortran project

BUILD_DIR = build

# Source files
SRC = \
	utils.F90 \
	args.F90

# Object files
OBJS = $(SRC:.F90=.o)

# Static library
LIB = aoc.a

# Build rules
all: $(LIB)

args.o: utils.F90

$(LIB): $(OBJS)
	$(AR) -crs $@ $^

%.o: %.F90
	mkdir -p $(BASE_BUILD_DIR)
	#mkdir -p $(BUILD_DIR)
	$(FC) $(FFLAGS) -c $< -o $@

clean:
	rm -rf $(BUILD_DIR)
	rm -f $(OBJS) $(LIB)

