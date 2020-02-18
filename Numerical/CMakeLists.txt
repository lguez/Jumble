set(file_list argwhere.f90 averge.f90 differ_s.f90 divisors.f90 ediff1d.f90
  pack_indices.f90 quadrat.f90 spherical.f90)

list(TRANSFORM file_list PREPEND ${CMAKE_CURRENT_LIST_DIR}/)
target_sources(jumble PRIVATE ${file_list})
include(${CMAKE_CURRENT_LIST_DIR}/Lin_2d_real/CMakeLists.txt)
