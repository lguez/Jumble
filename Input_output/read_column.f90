module read_column_m

  use, intrinsic:: ISO_FORTRAN_ENV

  use count_lines_m, only: count_lines
  use new_unit_m, only: new_unit
  use opt_merge_m, only: opt_merge

  implicit none

  private
  public read_column

  interface read_column
     ! This generic procedure reads a column of values in an external
     ! file. The file should contain a single column. The records of
     ! the file shoud be formatted. If the argument "nrows" is absent
     ! then the procedure reads to the last line in the file. The
     ! difference between the specific procedures is the type of
     ! argument "a".

     ! subroutine read_column(a, file, skiprows, nrows, my_lbound)
     ! real or integer or character(len=*), allocatable, intent(out):: a(:)
     ! character(len=*), intent(in):: file

     ! integer, intent(in), optional:: skiprows
     ! number of lines to skip at the start of the file, should be >= 0

     ! integer, intent(in), optional:: nrows
     ! Number of rows of file to read. If nrows <= 0 then a zero-sized
     ! array is returned in `a`.

     ! integer, optional, intent(in):: my_lbound ! lower bound of argument "a"

     module procedure read_column_real, read_column_integer, read_column_char
  end interface read_column

contains

  subroutine read_column_real(a, file, skiprows, nrows, my_lbound, usecol)

    real, allocatable, intent(out):: a(:)
    character(len=*), intent(in):: file
    integer, intent(in), optional:: skiprows
    integer, intent(in), optional:: nrows
    integer, optional, intent(in):: my_lbound
    integer, optional, intent(in):: usecol

    ! Variables local to the subprogram:
    integer unit ! external file unit
    integer skiprows_not_opt
    integer nrows_not_opt, usecol_not_opt
    integer my_lbound_not_opt ! lower bound of argument "a", local variable
    integer i, j
    character trash

    !------------------------------------------------------

    call new_unit(unit)
    open(unit, file = file, status = 'old', action = 'read', &
         position = 'rewind')
    include "read_column.h"
    close(unit)

  end subroutine read_column_real

  !***********************************************************

  subroutine read_column_integer(a, file, skiprows, nrows, my_lbound, usecol)

    integer, allocatable, intent(out):: a(:)
    character(len=*), intent(in):: file
    integer, intent(in), optional:: skiprows
    integer, intent(in), optional:: nrows
    integer, optional, intent(in):: my_lbound
    integer, optional, intent(in):: usecol

    ! Variables local to the subprogram:
    integer unit ! external file unit
    integer skiprows_not_opt
    integer nrows_not_opt, usecol_not_opt
    integer my_lbound_not_opt ! lower bound of argument "a", local variable
    integer i, j
    character trash

    !------------------------------------------------------

    call new_unit(unit)
    open(unit, file = file, status = 'old', action = 'read', &
         position = 'rewind')
    include "read_column.h"
    close(unit)

  end subroutine read_column_integer

  !***********************************************************

  subroutine read_column_char(a, file, skiprows, nrows, my_lbound, usecol)

    character(len=*), allocatable, intent(out):: a(:)
    character(len=*), intent(in):: file
    integer, intent(in), optional:: skiprows
    integer, intent(in), optional:: nrows
    integer, optional, intent(in):: my_lbound
    integer, optional, intent(in):: usecol

    ! Variables local to the subprogram:
    integer unit ! external file unit
    integer skiprows_not_opt
    integer nrows_not_opt, usecol_not_opt
    integer my_lbound_not_opt ! lower bound of argument "a", local variable
    integer i, j
    character trash

    !------------------------------------------------------

    call new_unit(unit)
    open(unit, file = file, status = 'old', action = 'read', &
         position = 'rewind')
    include "read_column.h"
    close(unit)

  end subroutine read_column_char

end module read_column_m
