program test_read_column

  use jumble, only: read_column

  implicit none

  real, allocatable:: a(:)

  !-----------------------------------------------------------------------

  call read_column(a, "test_read_column.txt")
  print *, "a = ", a
  call read_column(a, "test_read_column.txt", skiprows = 1)
  print *, "a = ", a
  call read_column(a, "test_read_column.txt", nrows = 1)
  print *, "a = ", a
  call read_column(a, "test_read_column.txt", skiprows = 1, nrows = 1)
  print *, "a = ", a
  call read_column(a, "test_csvread.csv", usecol = 2)
  print *, "a = ", a

end program test_read_column
