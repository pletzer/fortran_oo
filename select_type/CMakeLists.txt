include_directories(
  ${CMAKE_CURRENT_SOURCE_DIR}
  )

set(LIB_FILES
  test_select_type.f90
)

set(HEADER_FILES
)

add_executable(test_select_type test_select_type.f90)

add_test(NAME test_select_type
         COMMAND test_select_type)
set_tests_properties(test_select_type PROPERTIES
                     PASS_REGULAR_EXPRESSION "Rectangle area:")


# Install headers
install(FILES ${HEADER_FILES} DESTINATION include)

