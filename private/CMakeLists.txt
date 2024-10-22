include_directories(
  ${CMAKE_CURRENT_SOURCE_DIR}
  )

set(LIB_FILES
  private_mod.f90
)

set(HEADER_FILES
)

add_library(f_oo_private ${LIB_FILES})

add_executable(test_private test_private.f90)
target_link_libraries(test_private f_oo_private)

add_test(NAME test_private
         COMMAND test_private)

# Install headers
install(FILES ${HEADER_FILES} DESTINATION include)

# Install library
install(TARGETS f_oo_private DESTINATION lib)
