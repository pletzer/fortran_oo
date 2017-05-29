include_directories(
  ${CMAKE_CURRENT_SOURCE_DIR}
  )

set(LIB_FILES
  list_mod.f90
  list_poly_mod.f90
)

set(HEADER_FILES
)

add_library(f_oo_list ${LIB_FILES})

add_executable(test_list test_list.f90)
target_link_libraries(test_list f_oo_list)

add_executable(test_list_poly test_list_poly.f90)
target_link_libraries(test_list_poly f_oo_list)


add_test(NAME test_list 
         COMMAND test_list)

add_test(NAME test_list_poly
         COMMAND test_list_poly)


# Install headers
install(FILES ${HEADER_FILES} DESTINATION include)

# Install library
install(TARGETS f_oo_list DESTINATION lib)
