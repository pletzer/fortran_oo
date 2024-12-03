include_directories(
  ${CMAKE_CURRENT_SOURCE_DIR}
  )

set(LIB_FILES
  test_polymorphism.f90
)

set(HEADER_FILES
)

add_executable(test_polymorphism test_polymorphism.f90)

add_test(NAME test_polymorphism
         COMMAND test_polymorphism)
set_tests_properties(test_polymorphism PROPERTIES
                     PASS_REGULAR_EXPRESSION "Rectangle area:")


# Install headers
install(FILES ${HEADER_FILES} DESTINATION include)

