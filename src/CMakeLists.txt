
set(PROJECT "aoc-fortran")

project(${PROJECT} Fortran)

set(SRCS
	args.F90
	blarg.F90
	map.F90
	map_i32.F90
	map_i64.F90
	map_str.F90
	utils.F90
)

# Set module output dir
add_compile_options("-J${CMAKE_CURRENT_SOURCE_DIR}/build/${CMAKE_BUILD_TYPE}")

add_library(${PROJECT} ${SRCS})

