
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

add_compile_options("-J${CMAKE_CURRENT_SOURCE_DIR}/build/")
add_library(${PROJECT} ${SRCS})

