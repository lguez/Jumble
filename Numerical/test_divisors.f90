program test_divisors

  use jumble, only: divisors, get_command_arg_dyn

  implicit none

  integer n
  character(len = :), allocatable:: arg_val

  !-----------------------------------------------

  call get_command_arg_dyn(1, arg_val, "Required argument: n")
  read(unit = arg_val, fmt = *) n
  print *, "divisors:", divisors(n)

end program test_divisors
