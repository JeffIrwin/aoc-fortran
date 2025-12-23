cmake_minimum_required(VERSION 3.22)

set(PROJECT "main")
project(${PROJECT} Fortran)

set(SRCS
	main.F90
)

if(NOT CMAKE_BUILD_TYPE)
	set(CMAKE_BUILD_TYPE "Debug" CACHE STRING "Choose Release or Debug" FORCE)
endif()

# TODO: set fortran flags depending on build type. And just use
# add_compile_options() instead of manually setting CMAKE_Fortran_FLAGS_DEBUG
set(CMAKE_Fortran_FLAGS_DEBUG "-g -O0 -fcheck=all -fimplicit-none -Werror=implicit-interface")

set(LIB_DIR "${CMAKE_SOURCE_DIR}/../../src")
set(LIB_BUILD_DIR "${LIB_DIR}/build/${CMAKE_BUILD_TYPE}")
add_subdirectory(${LIB_DIR} ${LIB_BUILD_DIR})

# Set module include dir
add_compile_options("-I${LIB_BUILD_DIR}")

# Put exe in current dir
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

add_executable(${PROJECT} ${SRCS})
target_link_libraries(${PROJECT} aoc-fortran)

