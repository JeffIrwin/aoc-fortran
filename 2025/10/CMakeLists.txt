
set(PROJECT "main")

project(${PROJECT} Fortran)

set(SRCS
	main.F90
)

set(LIB_BUILD_DIR "${CMAKE_SOURCE_DIR}/../../src/build")

#add_subdirectory("${CMAKE_SOURCE_DIR}/../../src")
add_subdirectory("${CMAKE_SOURCE_DIR}/../../src" ${LIB_BUILD_DIR})

#target_include_directories(${PROJECT} PUBLIC
#	${LIB_BUILD_DIR}
#)
add_compile_options(
	#"-J${LIB_BUILD_DIR}"
	"-I${LIB_BUILD_DIR}"
)

add_executable(${PROJECT} ${SRCS})

target_link_libraries(${PROJECT}
	#${LIB_BUILD_DIR}/libaoc-fortran.a
	aoc-fortran
)

