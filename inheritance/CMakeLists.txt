include_directories(
  ${CMAKE_CURRENT_SOURCE_DIR}
  )

set(LIB_FILES
  test_inheritance.f90
)

set(HEADER_FILES
)

add_executable(test_inheritance test_inheritance.f90)

add_test(NAME test_inheritance
         COMMAND test_inheritance)
set_tests_properties(test_inheritance PROPERTIES
                     PASS_REGULAR_EXPRESSION "Finalizing base class"
                     PASS_REGULAR_EXPRESSION "Finalizing derived class")


# Install headers
install(FILES ${HEADER_FILES} DESTINATION include)

