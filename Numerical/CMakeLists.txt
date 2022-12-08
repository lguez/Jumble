target_sources(jumble PRIVATE argwhere.f90 averge.f90 differ_s.f90 avg_mag.f90
  divisors.f90 ediff1d.f90 pack_indices.f90 quadrat.f90 spherical.f90 mean.f90)

add_subdirectory(Lin_2d_real)

# test_quadrat
add_executable(test_quadrat EXCLUDE_FROM_ALL test_quadrat.f90)
target_link_libraries(test_quadrat PRIVATE jumble)

# test_divisors
add_executable(test_divisors EXCLUDE_FROM_ALL test_divisors.f90)
target_link_libraries(test_divisors PRIVATE jumble)
