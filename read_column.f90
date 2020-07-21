module read_column_m

  use count_lines_m, only: count_lines
  use new_unit_m, only: new_unit
  use opt_merge_m, only: opt_merge

  implicit none

  private
  public read_column

  interface read_column
     ! This generic procedure reads a column of values in an external
     ! file. The records of the file shoud be formatted. If the
     ! argument "last" is huge(0) or is absent then the procedure
     ! reads to the last line in the file. The difference between the
     ! specific procedures is the type of argument "a".
     module procedure read_column_real, read_column_integer, read_column_char
  end interface read_column

contains

  subroutine read_column_real(a, file, first, last)

    real, allocatable, intent(out):: a(:)
    character(len=*), intent(in):: file
    integer, intent(in), optional:: first ! (first line to read)
    integer, intent(in), optional:: last ! (last line to read)

    ! Variables local to the subprogram:
    integer unit ! external file unit
    integer first_not_opt ! first line to read, local variable
    integer last_not_opt ! last line to read, local variable
    integer i

    !------------------------------------------------------

    include "read_column.h"

  end subroutine read_column_real

  !***********************************************************

  subroutine read_column_integer(a, file, first, last)

    integer, allocatable, intent(out):: a(:)
    character(len=*), intent(in):: file
    integer, intent(in), optional:: first ! (first line to read)
    integer, intent(in), optional:: last ! (last line to read)

    ! Variables local to the subprogram:
    integer unit ! external file unit
    integer first_not_opt ! first line to read, local variable
    integer last_not_opt ! last line to read, local variable
    integer i

    !------------------------------------------------------

    include "read_column.h"

  end subroutine read_column_integer

  !***********************************************************

  subroutine read_column_char(a, file, first, last)

    character(len=*), allocatable, intent(out):: a(:)
    character(len=*), intent(in):: file
    integer, intent(in), optional:: first ! (first line to read)
    integer, intent(in), optional:: last ! (last line to read)

    ! Variables local to the subprogram:
    integer unit ! external file unit
    integer first_not_opt ! first line to read, local variable
    integer last_not_opt ! last line to read, local variable
    integer i

    !------------------------------------------------------

    include "read_column.h"

  end subroutine read_column_char

end module read_column_m
