
set(PROJECT "main")

project(${PROJECT} Fortran)

set(SRCS
	main.F90
)

set(LIB_DIR "${CMAKE_SOURCE_DIR}/../../src")
set(LIB_BUILD_DIR "${LIB_DIR}/build")
add_subdirectory(${LIB_DIR} ${LIB_BUILD_DIR})

# Set module include dir
add_compile_options("-I${LIB_BUILD_DIR}")

add_executable(${PROJECT} ${SRCS})
target_link_libraries(${PROJECT} aoc-fortran)

