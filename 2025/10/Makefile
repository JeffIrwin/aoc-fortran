
# This makefile builds a Fortran project

# Compiler
FC = gfortran

# Module output folder (for nvim linting)
BUILD = ./build/

# Compiler flags
FFLAGS = -Wall -Wextra -Wno-tabs -J$(BUILD)
FFLAGS += -fbounds-check -fbacktrace -g  # append debug flags
#FFLAGS += -O3  # append release flags

# Ignore incorrect warnings
FFLAGS += -Wno-maybe-uninitialized
FFLAGS += -Wno-uninitialized

# Source files
SRC = ../../utils.f90 main.f90

# Object files. TODO: change to uppercase .F90 extensions
OBJ = $(SRC:.f90=.o)

# Executable
EXEC = main

# Build rules
all: $(EXEC)

$(EXEC): $(OBJ)
	$(FC) $(FFLAGS) -o $@ $^

%.o: %.f90
	mkdir -p $(BUILD)
	$(FC) $(FFLAGS) -c $< -o $@

clean:
	rm -f $(OBJ) $(EXEC)
