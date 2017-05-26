include_directories(
  ${CMAKE_CURRENT_SOURCE_DIR}
  )

set(LIB_FILES
  list_poly_mod.f90
)

set(HEADER_FILES
)

add_library(f_oo_list_poly ${LIB_FILES})


add_executable(test_list_poly test_list_poly.f90)
target_link_libraries(test_list_poly f_oo_list_poly)

add_test(NAME test_list_poly 
         COMMAND test_list_poly)

# Install headers
install(FILES ${HEADER_FILES} DESTINATION include)

# Install library
install(TARGETS f_oo_list_poly DESTINATION lib)
