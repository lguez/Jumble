cmake_minimum_required(VERSION 3.16)
list(APPEND CMAKE_MESSAGE_CONTEXT Jumble)
project(Jumble LANGUAGES Fortran)
set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS Debug Release Profile)

add_library(jumble opt_merge.f90 uniq.f90 jumble.f90 prt_cmp.f90
  new_unit.f90 get_command_arg_dyn.f90 pr_matrix.f90 point.f90
  iso_varying_string.f90 csvread.f90 read_column.f90 count_lines.f90
  avg_mag.f90 compare.f90 greg2jd.f90 count_values.f90 csvread.h
  compare.h read_column.h)

set_target_properties(jumble PROPERTIES Fortran_MODULE_DIRECTORY
  ${PROJECT_BINARY_DIR}/modules)

target_include_directories(jumble PUBLIC
  $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/modules>)

if(CMAKE_Fortran_COMPILER_ID MATCHES "NAG")
  target_include_directories(jumble PRIVATE ${CMAKE_CURRENT_LIST_DIR})
endif()

add_subdirectory(Numerical)
add_subdirectory(NR_util)
add_library(${PROJECT_NAME}::jumble ALIAS jumble)

# test_greg2jd
add_executable(test_greg2jd test_greg2jd.f90)
target_link_libraries(test_greg2jd PRIVATE jumble)

# Start installation part

if(CMAKE_Fortran_COMPILER_ID MATCHES "NAG|Intel|PGI")
  set(install_include_dir include/${PROJECT_NAME})

  install(DIRECTORY ${PROJECT_BINARY_DIR}/modules/ DESTINATION
    ${install_include_dir})

else()
  set(install_include_dir include)
  install(FILES ${PROJECT_BINARY_DIR}/modules/jumble.mod TYPE INCLUDE)
endif()

install(TARGETS jumble EXPORT ${PROJECT_NAME}Targets INCLUDES
  DESTINATION ${install_include_dir})

install(EXPORT ${PROJECT_NAME}Targets DESTINATION
  lib/cmake/${PROJECT_NAME} NAMESPACE ${PROJECT_NAME}:: FILE
  ${PROJECT_NAME}Config.cmake)

# End installation part

export(EXPORT ${PROJECT_NAME}Targets NAMESPACE ${PROJECT_NAME}:: FILE
  ${PROJECT_NAME}Config.cmake)
