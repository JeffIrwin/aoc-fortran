
set(PROJECT "main")

project(${PROJECT} Fortran)

set(SRCS
	main.F90
)

add_executable(${PROJECT} ${SRCS})

#add_compile_options(-ffree-form)

target_include_directories(${PROJECT}
	#PUBLIC ${PROJECT_SOURCE_DIR}/include
	PUBLIC ../../src/build/
)

target_link_libraries(${PROJECT}
	#${CMAKE_SOURCE_DIR}/libmylib.a
	#../../src/build/libaoc-fortran.a
	${CMAKE_SOURCE_DIR}/../../src/build/libaoc-fortran.a
)

