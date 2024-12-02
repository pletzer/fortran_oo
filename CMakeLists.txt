###################################################
#
# CMakeLists.txt for simplexgrid
#
###################################################

cmake_minimum_required(VERSION 3.2)

# Project information
project(fortran_oo)
enable_language(Fortran)

enable_testing()

add_subdirectory(inheritance)
add_subdirectory(final)
add_subdirectory(private)
add_subdirectory(list)
add_subdirectory(file_object)
add_subdirectory(class)

