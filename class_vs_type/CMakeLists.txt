include_directories(
  ${CMAKE_CURRENT_SOURCE_DIR}
  )

set(LIB_FILES
  test_class_vs_type.f90
)

set(HEADER_FILES
)

add_executable(test_class_vs_type test_class_vs_type.f90)

add_test(NAME test_class_vs_type
         COMMAND test_class_vs_type)
set_tests_properties(test_class_vs_type PROPERTIES
                     PASS_REGULAR_EXPRESSION "destructor was called")


# Install headers
install(FILES ${HEADER_FILES} DESTINATION include)

