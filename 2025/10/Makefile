
# This makefile builds a Fortran project

# Compiler
export FC = gfortran

# Module output folder (for nvim linting)
export BASE_DIR = build
export DBG_DIR = $(BASE_DIR)/debug
export REL_DIR = $(BASE_DIR)/release

# Compiler flags
export FFLAGS = -Wall -Wextra -Wno-tabs  # warnings
FFLAGS += -Wno-maybe-uninitialized  # ignore incorrect warnings
FFLAGS += -Wno-uninitialized
FFLAGS += -J$(BASE_DIR)  # module output dir
FFLAGS += -I$(LIB_SRC_DIR)/$(BASE_DIR)  # include dir

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
OBJS_DBG = $(addprefix $(DBG_DIR)/, $(OBJS))
OBJS_REL = $(addprefix $(REL_DIR)/, $(OBJS))

# Executable
EXE_DBG = main
EXE_REL = mainr

# Advent of code library from top-level src dir
LIB_SRC_DIR = ../../src
#LIB = $(LIB_SRC_DIR)/$(BUILD_DIR)/aoc.a
LIB_DBG = $(LIB_SRC_DIR)/$(DBG_DIR)/aoc.a
LIB_REL = $(LIB_SRC_DIR)/$(REL_DIR)/aoc.a

#=======================================================================

# Build rules
all: debug

debug: DBG $(EXE_DBG)
release: REL $(EXE_REL)

DBG:
	$(eval FFLAGS += $(FFLAGS_DBG))
	$(eval export BUILD_DIR="$(DBG_DIR)")
	$(eval CONFIG = debug)

REL:
	$(eval FFLAGS += $(FFLAGS_REL))
	$(eval export BUILD_DIR="$(REL_DIR)")
	$(eval CONFIG = release)

$(LIB_DBG): $(LIB_SRC_DIR)
	$(MAKE) -C $(LIB_SRC_DIR) $(CONFIG)

$(LIB_REL): $(LIB_SRC_DIR)
	$(MAKE) -C $(LIB_SRC_DIR) $(CONFIG)

$(EXE_DBG): $(OBJS_DBG)
	$(FC) $(FFLAGS) -o $@ $(OBJS_DBG) $(LIB_DBG)

$(EXE_REL): $(OBJS_REL)
	$(FC) $(FFLAGS) -o $@ $(OBJS_REL) $(LIB_REL)

$(DBG_DIR)/%.o: %.F90 $(LIB_DBG)
	mkdir -p $(DBG_DIR)
	$(FC) $(FFLAGS) -c $< -o $@

$(REL_DIR)/%.o: %.F90 $(LIB_REL)
	mkdir -p $(REL_DIR)  # TODO: move to prep rule
	$(FC) $(FFLAGS) -c $< -o $@

clean:
	rm -rf $(BASE_DIR)
	rm -f $(EXE_DBG) $(EXE_REL)
	$(MAKE) clean -C $(LIB_SRC_DIR)

