module read_opcol_m

  use, intrinsic:: ISO_FORTRAN_ENV

  use count_lines_m, only: count_lines
  use opt_merge_m, only: opt_merge
  use assert_m, only: assert

  implicit none

  private
  public read_opcol

  interface read_opcol
     ! This generic procedure reads a column of values in an external
     ! file. The file should be already opened for formatted
     ! sequential access. The difference between the specific
     ! procedures is the type of argument "a".

     ! subroutine read_opcol_real(a, unit, skiprows, nrows, my_lbound, usecol)

     ! real or integer or character(len=*), allocatable, intent(out):: a(:)
     ! integer, intent(in):: unit

     ! integer, intent(in), optional:: skiprows

     ! Number of lines to skip at the start of the file, should be >=
     ! 0. Default value is 0.

     ! integer, intent(in), optional:: nrows

     ! Number of rows of file to read. If nrows <= 0 then a zero-sized
     ! array is returned in `a`. If nrows is absent then the procedure
     ! reads to the last line in the file.

     ! integer, optional, intent(in):: my_lbound
     ! Lower bound of argument "a". Default is 1.

     ! integer, optional, intent(in):: usecol

     ! Which column to read, with 1 being the first. Should be >=
     ! 1. Default is 1.

     module procedure read_opcol_real, read_opcol_integer, read_opcol_char
  end interface read_opcol

contains

  subroutine read_opcol_real(a, unit, skiprows, nrows, my_lbound, usecol)

    real, allocatable, intent(out):: a(:)
    integer, intent(in):: unit
    integer, intent(in), optional:: skiprows
    integer, intent(in), optional:: nrows
    integer, optional, intent(in):: my_lbound
    integer, optional, intent(in):: usecol

    ! Variables local to the subprogram:
    integer skiprows_not_opt, nrows_not_opt, usecol_not_opt, my_lbound_not_opt
    integer i, j
    logical opened
    character trash

    !------------------------------------------------------

    inquire(unit, opened = opened)
    call assert(opened, "jumble - read_opcol_real: unit must be opened")
    include "read_column.h"

  end subroutine read_opcol_real

  !***********************************************************

  subroutine read_opcol_integer(a, unit, skiprows, nrows, my_lbound, usecol)

    integer, allocatable, intent(out):: a(:)
    integer, intent(in):: unit
    integer, intent(in), optional:: skiprows
    integer, intent(in), optional:: nrows
    integer, optional, intent(in):: my_lbound
    integer, optional, intent(in):: usecol

    ! Variables local to the subprogram:
    integer skiprows_not_opt, nrows_not_opt, usecol_not_opt, my_lbound_not_opt
    integer i, j
    logical opened
    character trash

    !------------------------------------------------------

    inquire(unit, opened = opened)
    call assert(opened, "jumble - read_opcol_integer: unit must be opened")
    include "read_column.h"

  end subroutine read_opcol_integer

  !***********************************************************

  subroutine read_opcol_char(a, unit, skiprows, nrows, my_lbound, usecol)

    character(len=*), allocatable, intent(out):: a(:)
    integer, intent(in):: unit
    integer, intent(in), optional:: skiprows
    integer, intent(in), optional:: nrows
    integer, optional, intent(in):: my_lbound
    integer, optional, intent(in):: usecol

    ! Variables local to the subprogram:
    integer skiprows_not_opt, nrows_not_opt, usecol_not_opt, my_lbound_not_opt
    integer i, j
    logical opened
    character trash

    !------------------------------------------------------

    inquire(unit, opened = opened)
    call assert(opened, "jumble - read_opcol_char: unit must be opened")
    include "read_column.h"

  end subroutine read_opcol_char

end module read_opcol_m
