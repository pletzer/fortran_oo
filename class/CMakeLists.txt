include_directories(
  ${CMAKE_CURRENT_SOURCE_DIR}
  )

set(LIB_FILES
  myclass_mod.f90
  test_myclass.f90
)

set(HEADER_FILES
)

add_library(f_oo_myclass ${LIB_FILES})

add_executable(test_myclass test_myclass.f90)
target_link_libraries(test_myclass f_oo_myclass)

add_test(NAME test_myclass
         COMMAND test_myclass)
set_tests_properties(test_myclass PROPERTIES
                     PASS_REGULAR_EXPRESSION "constructor was called"
                     PASS_REGULAR_EXPRESSION "set method was called"
                     PASS_REGULAR_EXPRESSION "destructor was called")


# Install headers
install(FILES ${HEADER_FILES} DESTINATION include)

# Install library
install(TARGETS f_oo_myclass DESTINATION lib)
