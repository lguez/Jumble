module csvread_m

  use count_lines_m, only: count_lines
  use count_values_m, only: count_values
  use new_unit_m, only: new_unit
  use opt_merge_m, only: opt_merge

  implicit none

  private
  public csvread

  interface csvread
     ! Reads numeric values from a file. Values must be separated by
     ! comma and/or blanks. Values are read into a default real kind
     ! or double precision array. The last column and/or last row
     ! arguments may be 0. This is interpreted as "last in the
     ! file". The only difference between the interfaces of the
     ! specific procedures is the type of "a".
     
     ! character(len=*), intent(in):: file
     ! real or double precision, allocatable, intent(out):: a(:,:)
     ! integer, intent(in), optional:: first_r ! first row to read
     ! integer, intent(in), optional:: first_c ! first column to read
     ! integer, intent(in), optional:: last_r ! last row to read
     ! integer, intent(in), optional:: last_c ! last column to read

     module procedure csvread_sp, csvread_dp
  end interface csvread

contains

  subroutine csvread_sp(file, a, first_r, first_c, last_r, last_c, iostat)

    character(len=*), intent(in):: file
    real, allocatable, intent(out):: a(:,:)
    integer, intent(in), optional:: first_r
    integer, intent(in), optional:: first_c
    integer, intent(in), optional:: last_r
    integer, intent(in), optional:: last_c
    integer, intent(out), optional:: iostat

    ! Variables local to the subprogram:
    integer i, j, unit, iostat_loc
    integer f_r_loc ! first row to read, local variable
    integer f_c_loc ! first column to read, local variable
    integer l_r_loc ! last row to read, local variable
    integer l_c_loc ! last column to read, local variable
    character trash, iomsg*200

    !------------------------------------------------------

    include "csvread.h"

  end subroutine csvread_sp

  !***********************************************************

  subroutine csvread_dp(file, a, first_r, first_c, last_r, last_c, iostat)

    character(len=*), intent(in):: file
    double precision, allocatable, intent(out):: a(:,:)
    integer, intent(in), optional:: first_r
    integer, intent(in), optional:: first_c
    integer, intent(in), optional:: last_r
    integer, intent(in), optional:: last_c
    integer, intent(out), optional:: iostat

    ! Variables local to the subprogram:
    integer i, j, unit, iostat_loc
    integer f_r_loc ! first row to read, local variable
    integer f_c_loc ! first column to read, local variable
    integer l_r_loc ! last row to read, local variable
    integer l_c_loc ! last column to read, local variable
    character trash, iomsg*200

    !------------------------------------------------------

    include "csvread.h"

  end subroutine csvread_dp

end module csvread_m
