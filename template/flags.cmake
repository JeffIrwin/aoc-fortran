
if(NOT CMAKE_BUILD_TYPE)
	set(CMAKE_BUILD_TYPE "Debug" CACHE STRING "Choose Release or Debug" FORCE)
endif()

if (CMAKE_BUILD_TYPE STREQUAL "Release")
	add_compile_options(-O3)
else()  # Debug
	add_compile_options(-g -O0 -fbacktrace)
	add_compile_options(-fcheck=all -fcheck=bounds -fcheck=array-temps)
endif()

# Always-on compile options
add_compile_options(-Wall -Wextra -Wno-tabs)  # warnings
add_compile_options(-Wno-maybe-uninitialized)  # ignore incorrect warnings
add_compile_options(-Wno-uninitialized)
add_compile_options(-fimplicit-none -Werror=implicit-interface)

# Set module include dir
add_compile_options("-I${LIB_BUILD_DIR}")

