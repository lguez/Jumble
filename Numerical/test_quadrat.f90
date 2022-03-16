program test_quadrat

  use jumble, only: quadrat

  implicit none

  real a, b, c, delta, root(2)

  !--------------------------------------------

  print *, "a = ? "
  read *, a
  print *, "b = ? "
  read *, b
  print *, "c = ? "
  read *, c
  call quadrat(a, b, c, delta, root)
  
  if (delta < 0) then
     print *, "No real root."
  else if (delta == 0.) then
     print *, "Double root: ", root(1)
  else
     print *, "Roots: ", root
  end if

end program test_quadrat
