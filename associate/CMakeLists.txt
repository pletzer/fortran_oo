include_directories(
  ${CMAKE_CURRENT_SOURCE_DIR}
  )

set(LIB_FILES
  test_associate.f90
)

set(HEADER_FILES
)

add_executable(test_associate test_associate.f90)

add_test(NAME test_associate
         COMMAND test_associate)
set_tests_properties(test_associate PROPERTIES
                     PASS_REGULAR_EXPRESSION "Result:")

# Install headers
install(FILES ${HEADER_FILES} DESTINATION include)

