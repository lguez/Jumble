program test_read_column

  use jumble, only: read_column

  implicit none

  real, allocatable:: a(:)

  !-----------------------------------------------------------------------

  call read_column(a, "test_read_column.txt")
  print *, "a = ", a

end program test_read_column
