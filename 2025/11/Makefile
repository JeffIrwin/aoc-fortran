
# This makefile builds the daily AOC executable program

# Compiler
export FC = gfortran

# Module output folder (for nvim linting)
export BUILD_DIR = build

# Compiler flags
export FFLAGS = -Wall -Wextra -Wno-tabs  # warnings
FFLAGS += -Wno-maybe-uninitialized  # ignore incorrect warnings
FFLAGS += -Wno-uninitialized
FFLAGS += -J$(abspath $(LIB_SRC_DIR)/$(BUILD_DIR))  # shared module output dir
FFLAGS += -I$(abspath $(LIB_SRC_DIR)/$(BUILD_DIR))  # include dir

# Debug compile flags
FFLAGS_DBG = -fcheck=all -fcheck=bounds -fcheck=array-temps  # runtime checks
FFLAGS_DBG += -fbacktrace -g  # log stack traces when crashing

# Release compile flags
FFLAGS_REL += -O3  # optimize level 3

#=======================================================================

# Source file(s)
SRC = \
	main.F90

# Object files
OBJS = $(SRC:.F90=.o)

# Executable
EXE = main

# Advent of code library from top-level src dir
LIB_SRC_DIR = ../../src
LIB = $(LIB_SRC_DIR)/aoc.a

#=======================================================================
# Build rules

# Default to debug profile
all: debug

debug: DBG $(EXE)
release: REL $(EXE)

.PHONY: all clean debug release

DBG:
	$(eval FFLAGS += $(FFLAGS_DBG))
	$(eval CONFIG = debug)

REL:
	$(eval FFLAGS += $(FFLAGS_REL))
	$(eval CONFIG = release)

$(LIB): $(LIB_SRC_DIR)
	mkdir -p $(LIB_SRC_DIR)/$(BUILD_DIR)
	$(MAKE) -C $(LIB_SRC_DIR) $(CONFIG)

# I previously had separate binary output folders for objects and executables,
# but it required duplication of so many rules that my makefile was twice as
# long:
#
#     https://github.com/JeffIrwin/aoc-fortran/blob/50308f9f1ac47dbdcea00140a279f2dae0f1e7ac/2025/10/Makefile
#
# Now you just have to clean between switching debug/release profiles
#
$(EXE): $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS) $(LIB)

$(OBJS): $(LIB)

%.o: %.F90
	$(FC) $(FFLAGS) -c $< -o $@

clean:
	rm -rf $(BUILD_DIR)
	rm -f $(EXE) $(OBJS)
	$(MAKE) clean -C $(LIB_SRC_DIR)

