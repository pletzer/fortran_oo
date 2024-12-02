###################################################
#
# CMakeLists.txt for simplexgrid
#
###################################################

# Project information
project(fortran_oo)
enable_language(Fortran)

# Required version
cmake_minimum_required(VERSION 3.2)

enable_testing()

add_subdirectory(final)
add_subdirectory(private)
add_subdirectory(list)
add_subdirectory(file_object)
add_subdirectory(class)

