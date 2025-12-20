
# This makefile builds a Fortran project

# Source files
SRC = \
	utils.F90 \
	args.F90

# Object files
OBJS = $(SRC:.F90=.o)
OBJS_DBG = $(addprefix $(DBG_DIR)/, $(OBJS))
OBJS_REL = $(addprefix $(REL_DIR)/, $(OBJS))

# Static library
LIB = aoc.a
LIB_DBG = $(DBG_DIR)/$(LIB)
LIB_REL = $(REL_DIR)/$(LIB)

#=======================================================================

# Build rules
all: debug

$(DBG_DIR)/args.o: utils.F90
$(REL_DIR)/args.o: utils.F90

debug:   $(LIB_DBG)
release: $(LIB_REL)

$(LIB_DBG): $(OBJS_DBG)
	$(AR) -crs $(LIB_DBG) $(OBJS_DBG)

$(LIB_REL): $(OBJS_REL)
	$(AR) -crs $(LIB_REL) $(OBJS_REL)

$(DBG_DIR)/%.o: %.F90
	mkdir -p $(DBG_DIR)
	$(FC) $(FFLAGS) -c $< -o $@

$(REL_DIR)/%.o: %.F90
	mkdir -p $(REL_DIR)
	$(FC) $(FFLAGS) -c $< -o $@

clean:
	rm -rf $(BASE_DIR)

