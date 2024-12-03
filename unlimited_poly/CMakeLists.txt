include_directories(
  ${CMAKE_CURRENT_SOURCE_DIR}
  )

set(LIB_FILES
  test_unlimited_poly.f90
)

set(HEADER_FILES
)

add_executable(test_unlimited_poly test_unlimited_poly.f90)

add_test(NAME test_unlimited_poly
         COMMAND test_unlimited_poly)
set_tests_properties(test_unlimited_poly PROPERTIES
                     PASS_REGULAR_EXPRESSION "Hello, Fortran!")


# Install headers
install(FILES ${HEADER_FILES} DESTINATION include)

