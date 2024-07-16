program test_csvread

  use jumble, only: csvread, get_command_arg_dyn

  implicit none

  integer, allocatable:: a(:, :)
  character(len=:), allocatable:: fname
  real, allocatable:: x(:, :)

  !-------------------------------------------------------------------

  call get_command_arg_dyn(1, fname)
  call csvread(fname, x)
  print *, "x = ", x
  call csvread(fname, a)
  print *, "a = ", a

end program test_csvread
