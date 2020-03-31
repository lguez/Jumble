cmake_minimum_required(VERSION 3.13)
project(Jumble Fortran)

add_library(jumble opt_merge.f90 uniq.f90 jumble.f90 prt_cmp.f90
  new_unit.f90 get_command_arg_dyn.f90 pr_matrix.f90 point.f90
  iso_varying_string.f90 csvread.f90 read_column.f90 count_lines.f90
  avg_mag.f90 compare.f90 greg2jd.f90 count_values.f90)

set_target_properties(jumble PROPERTIES Fortran_MODULE_DIRECTORY
  ${PROJECT_BINARY_DIR}/modules)

target_include_directories(jumble PUBLIC
  $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/modules>)

if(CMAKE_Fortran_COMPILER_ID MATCHES NAG)
  target_include_directories(jumble PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
endif()

include(Numerical/CMakeLists.txt)

if(CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME)
  find_package(NR_util CONFIG REQUIRED)
endif()

target_link_libraries(jumble PUBLIC nr_util)

set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR})
include(ConfigureCompilerFlags)

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
  install(FILES ${PROJECT_BINARY_DIR}/modules/jumble.mod TYPE INCLUDE)
endif()

install(TARGETS jumble EXPORT ${PROJECT_NAME}Targets LIBRARY
  INCLUDES DESTINATION ${install_include_dir})
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Config.cmake
  DESTINATION lib/cmake/${PROJECT_NAME})
install(EXPORT ${PROJECT_NAME}Targets DESTINATION lib/cmake/${PROJECT_NAME})
export(EXPORT ${PROJECT_NAME}Targets)
