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
     ! sequential access. The file should contain a single column. If
     ! the argument "last" is huge(0) or is absent then the procedure
     ! reads to the last line in the file. The difference between the
     ! specific procedures is the type of argument "a".

     ! real or integer or character(len=*), allocatable, intent(out):: a(:)
     ! integer, intent(in):: unit
     ! integer, intent(in), optional:: first ! (first line to read)
     ! integer, intent(in), optional:: last ! (last line to read)
     ! integer, optional, intent(in):: my_lbound ! lower bound of argument "a"

     module procedure read_opcol_real, read_opcol_integer, read_opcol_char
  end interface read_opcol

contains

  subroutine read_opcol_real(a, unit, first, last, my_lbound)

    real, allocatable, intent(out):: a(:)
    integer, intent(in):: unit
    integer, intent(in), optional:: first
    integer, intent(in), optional:: last
    integer, optional, intent(in):: my_lbound

    ! Variables local to the subprogram:
    integer first_not_opt ! first line to read, local variable
    integer last_not_opt ! last line to read, local variable
    integer my_lbound_not_opt ! lower bound of argument "a", local variable
    integer i
    logical opened

    !------------------------------------------------------

    inquire(unit, opened = opened)
    call assert(opened, "jumble - read_opcol_real: unit must be opened")
    include "read_column.h"

  end subroutine read_opcol_real

  !***********************************************************

  subroutine read_opcol_integer(a, unit, first, last, my_lbound)

    integer, allocatable, intent(out):: a(:)
    integer, intent(in):: unit
    integer, intent(in), optional:: first
    integer, intent(in), optional:: last
    integer, optional, intent(in):: my_lbound

    ! Variables local to the subprogram:
    integer first_not_opt ! first line to read, local variable
    integer last_not_opt ! last line to read, local variable
    integer my_lbound_not_opt ! lower bound of argument "a", local variable
    integer i
    logical opened

    !------------------------------------------------------

    inquire(unit, opened = opened)
    call assert(opened, "jumble - read_opcol_integer: unit must be opened")
    include "read_column.h"

  end subroutine read_opcol_integer

  !***********************************************************

  subroutine read_opcol_char(a, unit, first, last, my_lbound)

    character(len=*), allocatable, intent(out):: a(:)
    integer, intent(in):: unit
    integer, intent(in), optional:: first
    integer, intent(in), optional:: last
    integer, optional, intent(in):: my_lbound

    ! Variables local to the subprogram:
    integer first_not_opt ! first line to read, local variable
    integer last_not_opt ! last line to read, local variable
    integer my_lbound_not_opt ! lower bound of argument "a", local variable
    integer i
    logical opened

    !------------------------------------------------------

    inquire(unit, opened = opened)
    call assert(opened, "jumble - read_opcol_char: unit must be opened")
    include "read_column.h"

  end subroutine read_opcol_char

end module read_opcol_m
