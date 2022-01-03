target_sources(jumble PRIVATE nrtype.F90 arth.f90 array_copy.f90
  swap.f90 reallocate.f90 assert.f90 assert_eq.f90 geop.f90 cumsum.f90
  poly.f90 poly_term.f90 outerprod.f90 outerdiff.f90 scatter_add.f90
  scatter_max.f90 diagadd.f90 diagmult.f90 get_diag.f90 put_diag.f90
  cumprod.f90 ifirstloc.f90 lower_triangle.f90 nrerror.f90
  outerand.f90 outerdiv.f90 outersum.f90 unit_matrix.f90
  upper_triangle.f90 vabs.f90 zroots_unity.f90 swap_array.h
  swap_scalar.h)

if(CMAKE_Fortran_COMPILER_ID MATCHES NAG)
  target_include_directories(jumble PRIVATE ${CMAKE_CURRENT_LIST_DIR})
endif()

# CPP_WP
set(Jumble_CPP_WP kind\(0.\) CACHE STRING "Working precision")
target_compile_definitions(jumble PRIVATE CPP_WP=${Jumble_CPP_WP})
message(STATUS "Working precision: CPP_WP = ${Jumble_CPP_WP}")

find_package(MPI)

if(MPI_Fortran_HAVE_F08_MODULE)
  target_link_libraries(jumble PRIVATE MPI::MPI_Fortran)
  target_compile_definitions(jumble PRIVATE HAVE_MPI)
endif()

# test_nr_util
add_executable(test_nr_util test_nr_util.f90)
target_link_libraries(test_nr_util PRIVATE jumble)

# test_ifirstloc
add_executable(test_ifirstloc test_ifirstloc.f90)
target_link_libraries(test_ifirstloc PRIVATE jumble)
