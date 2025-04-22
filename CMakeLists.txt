cmake_minimum_required(VERSION 3.16)
list(APPEND CMAKE_MESSAGE_CONTEXT Jumble)
set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build")
project(Jumble LANGUAGES Fortran)

if(CMAKE_Fortran_COMPILER_ID MATCHES Intel|IntelLLVM)
  if (CMAKE_Fortran_FLAGS)
    STRING(FIND ${CMAKE_Fortran_FLAGS} noold_maxminloc index)
  else()
    set(index -1)
  endif()

  if(${index} EQUAL -1)
    message(WARNING "We need -assume noold_maxminloc because of Intel compiler "
      "bug: "
      "https://community.intel.com/t5/Intel-Fortran-Compiler/Incorrect-minloc-maxloc-results-with-ifx-2024-0/m-p/1549287")
  endif()

  set(bug_address
    "https://community.intel.com/t5/Intel-Fortran-Compiler/Compiler-bug-backspace-after-non-advancing-read/m-p/1614679#M172939")

  if(CMAKE_Fortran_COMPILER_ID MATCHES IntelLLVM)
    if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 2024.2.0)
      message(WARNING "Procedure csvread does not work with ifx version "
	"2023.1.0 because of backspace bug in compiler: ${bug_address}"
      )
    endif()
  else()
    # CMAKE_Fortran_COMPILER_ID MATCHES Intel
    if(CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER 19.1.3.20200925)
      message(WARNING "Procedure csvread does not work with ifort version "
	"2021.9.0 because of backspace bug in compiler: ${bug_address}")
    endif()
  endif()
endif()

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

# test_count_substrings
add_executable(test_count_substrings EXCLUDE_FROM_ALL test_count_substrings.f90)
target_link_libraries(test_count_substrings PRIVATE jumble)

add_dependencies(tests_jumble test_greg2jd test_squeeze
  test_count_substrings)

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
