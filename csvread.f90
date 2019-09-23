module csvread_m

  use new_unit_m, only: new_unit
  use prep_file_m, only: prep_file

  implicit none

  private
  public csvread

  interface csvread
     ! Reads numeric values from a file. Values must be separated by
     ! comma and/or blanks. Values are read into a default real kind
     ! or double precision array. The last column and/or last row
     ! parameters may be 0. This is interpreted as "last in the
     ! file". The only difference between the interfaces of the
     ! specific procedures is the type of "a".
     module procedure csvread_sp, csvread_dp
  end interface csvread

contains

  subroutine csvread_sp(file, a, first_r, first_c, last_r, last_c)

    character(len=*), intent(in):: file
    real, allocatable, intent(out):: a(:,:)
    integer, intent(in), optional:: first_r ! (first row to read)
    integer, intent(in), optional:: first_c ! (first column to read)
    integer, intent(in), optional:: last_r ! (last row to read)
    integer, intent(in), optional:: last_c ! (last column to read)

    ! Variables local to the subprogram:
    integer i, j, unit
    integer f_r_loc ! (first row to read, local variable)
    integer f_c_loc ! (first column to read, local variable)
    integer l_r_loc ! (last row to read, local variable)
    integer l_c_loc ! (last column to read, local variable)
    character trash

    !------------------------------------------------------

    print *, 'Reading data from file "' // file // '"'
    call new_unit(unit)
    open(unit, file=file, status='old', action='read', position='rewind')

    call prep_file(unit, first_r, first_c, last_r, last_c, f_r_loc, &
         f_c_loc, l_r_loc, l_c_loc)

    allocate(a(l_r_loc - f_r_loc + 1, l_c_loc - f_c_loc + 1))

    do i = 1, l_r_loc - f_r_loc + 1
       read(unit, fmt=*) (trash, j = 1, f_c_loc - 1), a(i, :)
    end do

    close(unit)

  end subroutine csvread_sp

  !***********************************************************

  subroutine csvread_dp(file, a, first_r, first_c, last_r, last_c)

    character(len=*), intent(in):: file
    double precision, allocatable, intent(out):: a(:,:)
    integer, intent(in), optional:: first_r ! (first row to read)
    integer, intent(in), optional:: first_c ! (first column to read)
    integer, intent(in), optional:: last_r ! (last row to read)
    integer, intent(in), optional:: last_c ! (last column to read)

    ! Variables local to the subprogram:
    integer i, j, unit
    integer f_r_loc ! (first row to read, local variable)
    integer f_c_loc ! (first column to read, local variable)
    integer l_r_loc ! (last row to read, local variable)
    integer l_c_loc ! (last column to read, local variable)
    character trash

    !------------------------------------------------------

    print *, 'Reading data from file "' // file // '"'
    call new_unit(unit)
    open(unit, file=file, status='old', action='read', position='rewind')

    call prep_file(unit, first_r, first_c, last_r, last_c, f_r_loc, &
         f_c_loc, l_r_loc, l_c_loc)

    allocate(a(l_r_loc - f_r_loc + 1, l_c_loc - f_c_loc + 1))

    do i = 1, l_r_loc - f_r_loc + 1
       read(unit, fmt=*) (trash, j = 1, f_c_loc - 1), a(i, :)
    end do

    close(unit)

  end subroutine csvread_dp

end module csvread_m
