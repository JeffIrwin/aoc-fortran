
# This makefile builds a Fortran project

# Compiler
export FC = gfortran

# Module output folder (for nvim linting)
export BASE_BUILD_DIR = build
export DEBUG_DIR = $(BASE_BUILD_DIR)/debug
export RELEASE_DIR = $(BASE_BUILD_DIR)/release
BUILD_DIR = dummy

# Compiler flags
export FFLAGS = -Wall -Wextra -Wno-tabs  # warnings
FFLAGS += -Wno-maybe-uninitialized  # ignore incorrect warnings
FFLAGS += -Wno-uninitialized

#FFLAGS += -J$(BUILD_DIR)  # module output dir
FFLAGS += -J$(BASE_BUILD_DIR)  # module output dir

#FFLAGS += -I$(LIB_SRC_DIR)/$(BUILD_DIR)  # include dir
FFLAGS += -I$(LIB_SRC_DIR)/$(BASE_BUILD_DIR)  # include dir

# Debug compile flags
FFLAGS_DEBUG = -fcheck=all -fcheck=bounds -fcheck=array-temps  # runtime checks
FFLAGS_DEBUG += -fbacktrace -g  # log stack traces when crashing

# Release compile flags
FFLAGS_RELEASE += -O3  # optimize level 3

#=======================================================================
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

#=======================================================================
# Build rules
all: $(EXE)

# TODO: default to debug
debug: DEBUG all
release: RELEASE all

DEBUG:
	$(eval FFLAGS += $(FFLAGS_DEBUG))
	$(eval BUILD_DIR="$(DEBUG_DIR)")

RELEASE:
	$(eval FFLAGS += $(FFLAGS_RELEASE))
	$(eval BUILD_DIR="$(RELEASE_DIR)")

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

