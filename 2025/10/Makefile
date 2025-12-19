
# This makefile builds a Fortran project

# Compiler
export FC = gfortran

# Module output folder (for nvim linting)
export BUILD_DIR = ./build/

# Compiler flags
export FFLAGS = -Wall -Wextra -Wno-tabs
FFLAGS += -J$(BUILD_DIR)
FFLAGS += -I$(LIB_SRC_DIR)/$(BUILD_DIR)

# append debug flags
FFLAGS += -fcheck=all -fcheck=bounds -fcheck=array-temps
FFLAGS += -fbacktrace -g

## append release flags
#FFLAGS += -O3

# Ignore incorrect warnings
FFLAGS += -Wno-maybe-uninitialized
FFLAGS += -Wno-uninitialized

# Source file(s)
SRC = \
	main.F90

# Object files
OBJ = $(SRC:.F90=.o)

# Executable
EXE = main

# Advent of code library from top-level src dir
LIB_SRC_DIR = ../../src
LIB = $(LIB_SRC_DIR)/aoc.a

# Build rules
all: $(EXE)

main.o: $(LIB)

$(LIB): $(LIB_SRC_DIR)
	$(MAKE) -C $(LIB_SRC_DIR)

$(EXE): $(OBJ) $(LIB)
	$(FC) $(FFLAGS) -o $@ $^

%.o: %.F90
	mkdir -p $(BUILD_DIR)
	$(FC) $(FFLAGS) -c $< -o $@

clean:
	rm -rf $(BUILD_DIR)
	rm -f $(OBJ) $(EXE)
	$(MAKE) clean -C $(LIB_SRC_DIR)

