include_directories(
  ${CMAKE_CURRENT_SOURCE_DIR}
  )

set(LIB_FILES
  test_class2.f90
)

set(HEADER_FILES
)

add_executable(test_class2 test_class2.f90)

add_test(NAME test_class2
         COMMAND test_class2)
set_tests_properties(test_class2 PROPERTIES
                     PASS_REGULAR_EXPRESSION "destructor was called")


# Install headers
install(FILES ${HEADER_FILES} DESTINATION include)

