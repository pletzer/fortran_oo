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

add_subdirectory(polymorphism2)
add_subdirectory(class2)
add_subdirectory(generic_type_bound)
add_subdirectory(specific_type_bound)
add_subdirectory(select_type)
add_subdirectory(associate)
add_subdirectory(unlimited_poly)
add_subdirectory(polymorphism)
add_subdirectory(inheritance)
add_subdirectory(final)
add_subdirectory(private)
add_subdirectory(list)
add_subdirectory(file_object)
add_subdirectory(class)

