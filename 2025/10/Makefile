
# This makefile builds a Fortran project

# Compiler
FC = gfortran

# Module output folder (for nvim linting)
BUILD = ./build/

# Compiler flags
FFLAGS = -Wall -Wextra -Wno-tabs -J$(BUILD)
FFLAGS += -fcheck=all -fcheck=bounds -fcheck=array-temps -fbacktrace -g  # append debug flags
#FFLAGS += -O3  # append release flags

# Ignore incorrect warnings
FFLAGS += -Wno-maybe-uninitialized
FFLAGS += -Wno-uninitialized

# Source files
SRC = \
	  ../../src/utils.F90 \
	  ../../src/args.F90 \
	  main.F90

# Object files
OBJ = $(SRC:.F90=.o)

# Executable
EXEC = main

# Build rules
all: $(EXEC)

../../src/args.o: ../../src/utils.F90

main.o: ../../src/utils.F90 ../../src/args.F90

$(EXEC): $(OBJ)
	$(FC) $(FFLAGS) -o $@ $^

%.o: %.F90
	mkdir -p $(BUILD)
	$(FC) $(FFLAGS) -c $< -o $@

clean:
	rm -f $(OBJ) $(EXEC)
