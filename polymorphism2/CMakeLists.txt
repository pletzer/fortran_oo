include_directories(
  ${CMAKE_CURRENT_SOURCE_DIR}
  )

set(LIB_FILES
  test_polymorphism2.f90
)

set(HEADER_FILES
)

add_executable(test_polymorphism2 test_polymorphism2.f90)

add_test(NAME test_polymorphism2
         COMMAND test_polymorphism2)
set_tests_properties(test_polymorphism2 PROPERTIES
                     PASS_REGULAR_EXPRESSION "Rectangle area:")


# Install headers
install(FILES ${HEADER_FILES} DESTINATION include)

