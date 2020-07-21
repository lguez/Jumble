program test_eigval

  use jumble, only: eigval

  implicit none

  real a(2, 2)
  integer n
  real lambda(2)

  !------------------------------------------------------------------------

  print *, "Enter a, line by line:"
  read *, a(1, :)
  read *, a(2, :)
  call eigval(a, n, lambda)
  print *, "n = ", n
  if (n == 1) then
     print *, "lambda = ", lambda(1)
  else if (n == 2) then
     print *, "lambda = ", lambda
  end if
  
end program test_eigval
