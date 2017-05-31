include_directories(
  ${CMAKE_CURRENT_SOURCE_DIR}
  )

set(LIB_FILES
  file_object_mod.f90
  test_file_object.f90
)

set(HEADER_FILES
)

add_library(f_oo_file_object ${LIB_FILES})

add_executable(file_object_driver file_object_driver.f90)
target_link_libraries(file_object_driver f_oo_file_object)

add_test(NAME file_object_driver
         COMMAND file_object_driver)

# Install headers
install(FILES ${HEADER_FILES} DESTINATION include)

# Install library
install(TARGETS f_oo_file_object DESTINATION lib)
