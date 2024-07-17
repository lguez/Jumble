cmake_minimum_required(VERSION 3.16)
list(APPEND CMAKE_MESSAGE_CONTEXT Jumble)
set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build")
project(Jumble LANGUAGES Fortran)
set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS Debug Release Profile)
add_library(jumble opt_merge.f90 uniq.f90 jumble.f90 point.f90
  iso_varying_string.f90 greg2jd.f90 squeeze.f90 count_substrings.f90)
set_target_properties(jumble PROPERTIES Fortran_MODULE_DIRECTORY
  ${PROJECT_BINARY_DIR}/modules)
target_include_directories(jumble PUBLIC
  $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/modules>)

if(CMAKE_Fortran_COMPILER_ID MATCHES "NAG")
  target_include_directories(jumble PRIVATE ${CMAKE_CURRENT_LIST_DIR})
endif()

add_custom_target(tests_jumble)
add_subdirectory(Numerical)
add_subdirectory(NR_util)
add_subdirectory(Input_output)
add_library(${PROJECT_NAME}::jumble ALIAS jumble)

# test_greg2jd
add_executable(test_greg2jd EXCLUDE_FROM_ALL test_greg2jd.f90)
target_link_libraries(test_greg2jd PRIVATE jumble)

# test_squeeze
add_executable(test_squeeze EXCLUDE_FROM_ALL test_squeeze.f90)
target_link_libraries(test_squeeze PRIVATE jumble)

add_dependencies(tests_jumble test_greg2jd test_squeeze)

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

set(CPACK_PACKAGE_NAME ${PROJECT_NAME})
set(CPACK_PACKAGE_INSTALL_DIRECTORY ${CPACK_PACKAGE_NAME})
set(CPACK_VERBATIM_VARIABLES TRUE)
include(CPack)
