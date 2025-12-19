
# This makefile builds a Fortran project

# Source files
SRC = \
	utils.F90 \
	args.F90

# Object files
OBJS = $(SRC:.F90=.o)

# Static library
LIB = aoc.a

#=======================================================================

# Build rules
all: debug

## Build rules
#all: $(LIB)

#args.o: utils.F90
$(BUILD_DIR)/args.o: utils.F90

debug: DEBUG $(LIB)
release: RELEASE $(LIB)

DEBUG:
	$(eval OBJS = $(addprefix   $(DEBUG_DIR)/, $(OBJS)))
	$(eval LIB  = $(addprefix   $(DEBUG_DIR)/, $(LIB)))
	$(eval FFLAGS += $(FFLAGS_DEBUG))
	$(eval BUILD_DIR="$(DEBUG_DIR)")

RELEASE:
	$(eval OBJS = $(addprefix   $(RELEASE_DIR)/, $(OBJS)))
	$(eval LIB  = $(addprefix   $(RELEASE_DIR)/, $(LIB)))
	$(eval FFLAGS += $(FFLAGS_RELEASE))
	$(eval BUILD_DIR="$(RELEASE_DIR)")

#main.o: $(LIB)

#$(LIB): $(LIB_SRC_DIR)
#	$(MAKE) -C $(LIB_SRC_DIR)
#
#$(EXE): $(OBJS) $(LIB)
#	$(FC) $(FFLAGS) -o $@ $(OBJS) $(LIB)

$(LIB): $(OBJS)
	$(AR) -crs $(LIB) $(OBJS)

%.o: %.F90
	mkdir -p $(BUILD_DIR)
	$(FC) $(FFLAGS) -c $< -o $(BUILD_DIR)/$@

clean:
	rm -rf $(BASE_BUILD_DIR)

