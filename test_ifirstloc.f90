program test_ifirstloc

  use nr_util, only: ifirstloc

  implicit none

  real x(4)

  !----------------------------------------------------------------------

  x = [-0.26715985, -0.27890369,  2.44455113,  0.58038481]
  print *, "Subscript of first value >= 0.3:", ifirstloc(x >= 0.3)

end program test_ifirstloc
