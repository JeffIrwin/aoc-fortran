
cmake_minimum_required(VERSION 3.22)

set(PROJECT "main")
set(CONV "convert")
project(${PROJECT} Fortran)

set(SRCS
	main.F90
)
set(CONV_SRCS
	convert.F90
)

set(LIB_DIR "${CMAKE_SOURCE_DIR}/../../src")
set(LIB_BUILD_DIR "${LIB_DIR}/build/${CMAKE_BUILD_TYPE}")
add_subdirectory(${LIB_DIR} ${LIB_BUILD_DIR})

add_subdirectory(../../json-fortran/ ${CMAKE_SOURCE_DIR}/../../json-fortran/build)
#add_subdirectory(../../json-fortran/ ${LIB_BUILD_DIR})  # can't share build dir for two different source dirs

include(../../template/flags.cmake)

# Put exe in current dir
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

add_executable(${PROJECT} ${SRCS})
add_executable(${CONV}    ${CONV_SRCS})

# gomp is for OpenMP. TODO: add to template?
target_link_libraries(${PROJECT} aoc-fortran gomp)
target_link_libraries(${CONV} aoc-fortran gomp)
target_link_libraries(${CONV} jsonfortran-static)

