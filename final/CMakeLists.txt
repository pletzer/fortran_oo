include_directories(
  ${CMAKE_CURRENT_SOURCE_DIR}
  )

set(LIB_FILES
  test_final.f90
)

set(HEADER_FILES
)

add_executable(test_final test_final.f90)

add_test(NAME test_final
         COMMAND test_final)
set_tests_properties(test_final PROPERTIES
                     PASS_REGULAR_EXPRESSION "Finalizing my_type")


# Install headers
install(FILES ${HEADER_FILES} DESTINATION include)

