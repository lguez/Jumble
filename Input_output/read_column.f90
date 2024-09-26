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
     ! the file shoud be formatted. If the argument "last" is huge(0)
     ! or is absent then the procedure reads to the last line in the
     ! file. The difference between the specific procedures is the
     ! type of argument "a".

     ! real or integer or character(len=*), allocatable, intent(out):: a(:)
     ! character(len=*), intent(in):: file
     ! integer, intent(in), optional:: first ! (first line to read)
     ! integer, intent(in), optional:: last ! (last line to read)
     ! integer, optional, intent(in):: my_lbound ! lower bound of argument "a"

     module procedure read_column_real, read_column_integer, read_column_char
  end interface read_column

contains

  subroutine read_column_real(a, file, first, last, my_lbound)

    real, allocatable, intent(out):: a(:)
    character(len=*), intent(in):: file
    integer, intent(in), optional:: first
    integer, intent(in), optional:: last
    integer, optional, intent(in):: my_lbound

    ! Variables local to the subprogram:
    integer unit ! external file unit
    integer first_not_opt ! first line to read, local variable
    integer last_not_opt ! last line to read, local variable
    integer my_lbound_not_opt ! lower bound of argument "a", local variable
    integer i

    !------------------------------------------------------

    call new_unit(unit)
    open(unit, file = file, status = 'old', action = 'read', &
         position = 'rewind')
    include "read_column.h"
    close(unit)

  end subroutine read_column_real

  !***********************************************************

  subroutine read_column_integer(a, file, first, last, my_lbound)

    integer, allocatable, intent(out):: a(:)
    character(len=*), intent(in):: file
    integer, intent(in), optional:: first
    integer, intent(in), optional:: last
    integer, optional, intent(in):: my_lbound

    ! Variables local to the subprogram:
    integer unit ! external file unit
    integer first_not_opt ! first line to read, local variable
    integer last_not_opt ! last line to read, local variable
    integer my_lbound_not_opt ! lower bound of argument "a", local variable
    integer i

    !------------------------------------------------------

    call new_unit(unit)
    open(unit, file = file, status = 'old', action = 'read', &
         position = 'rewind')
    include "read_column.h"
    close(unit)

  end subroutine read_column_integer

  !***********************************************************

  subroutine read_column_char(a, file, first, last, my_lbound)

    character(len=*), allocatable, intent(out):: a(:)
    character(len=*), intent(in):: file
    integer, intent(in), optional:: first
    integer, intent(in), optional:: last
    integer, optional, intent(in):: my_lbound

    ! Variables local to the subprogram:
    integer unit ! external file unit
    integer first_not_opt ! first line to read, local variable
    integer last_not_opt ! last line to read, local variable
    integer my_lbound_not_opt ! lower bound of argument "a", local variable
    integer i

    !------------------------------------------------------

    call new_unit(unit)
    open(unit, file = file, status = 'old', action = 'read', &
         position = 'rewind')
    include "read_column.h"
    close(unit)

  end subroutine read_column_char

end module read_column_m
