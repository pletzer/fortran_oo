include_directories(
  ${CMAKE_CURRENT_SOURCE_DIR}
  )

set(LIB_FILES
  test_specific_type_bound.f90
)

set(HEADER_FILES
)

add_executable(test_specific_type_bound test_specific_type_bound.f90)

add_test(NAME test_specific_type_bound
         COMMAND test_specific_type_bound)
set_tests_properties(test_specific_type_bound PROPERTIES
                     PASS_REGULAR_EXPRESSION "Rectangle area:")


# Install headers
install(FILES ${HEADER_FILES} DESTINATION include)

