cmake_minimum_required(VERSION 3.14)
project(NR_util Fortran)

add_library(nr_util nrtype.F90 nr_util.f90 arth.f90 array_copy.f90
  swap.f90 reallocate.f90 assert.f90 assert_eq.f90 geop.f90 cumsum.f90
  poly.f90 poly_term.f90 outerprod.f90 outerdiff.f90 scatter_add.f90
  scatter_max.f90 diagadd.f90 diagmult.f90 get_diag.f90 put_diag.f90
  cumprod.f90 ifirstloc.f90 lower_triangle.f90 nrerror.f90
  outerand.f90 outerdiv.f90 outersum.f90 unit_matrix.f90
  upper_triangle.f90 vabs.f90 zroots_unity.f90)

set_target_properties(nr_util PROPERTIES Fortran_MODULE_DIRECTORY
  ${PROJECT_BINARY_DIR}/modules)
target_include_directories(nr_util PUBLIC
  $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/modules>)

if(CMAKE_Fortran_COMPILER_ID MATCHES NAG)
  target_include_directories(nr_util PRIVATE ${CMAKE_CURRENT_LIST_DIR})
endif()

set(NR_util_CPP_WP "kind\(0.\)" CACHE STRING "Working precision")
target_compile_definitions(nr_util PRIVATE CPP_WP=${NR_util_CPP_WP})
message(STATUS "${CMAKE_CURRENT_LIST_FILE}")
message(STATUS "-- Set working precision to ${NR_util_CPP_WP}")

set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR})
include(ConfigureCompilerFlags)

add_executable(test_nr_util test_nr_util.f90)
target_link_libraries(test_nr_util PRIVATE nr_util)

add_executable(test_ifirstloc test_ifirstloc.f90)
target_link_libraries(test_ifirstloc PRIVATE nr_util)

include(CMakePackageConfigHelpers)
configure_package_config_file(Config.cmake.in
    ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Config.cmake
    INSTALL_DESTINATION lib/cmake/${PROJECT_NAME})

if(CMAKE_Fortran_COMPILER_ID MATCHES NAG|Intel)
  set(install_include_dir include/${PROJECT_NAME})
  install(DIRECTORY ${PROJECT_BINARY_DIR}/modules/ DESTINATION
    ${install_include_dir})
else()
  set(install_include_dir include)
  install(FILES ${PROJECT_BINARY_DIR}/modules/nr_util.mod TYPE INCLUDE)
endif()

install(TARGETS nr_util EXPORT ${PROJECT_NAME}Targets LIBRARY
  INCLUDES DESTINATION ${install_include_dir})
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Config.cmake
  DESTINATION lib/cmake/${PROJECT_NAME})
install(EXPORT ${PROJECT_NAME}Targets DESTINATION lib/cmake/${PROJECT_NAME})
export(EXPORT ${PROJECT_NAME}Targets)
