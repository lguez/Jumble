module read_column_m

  implicit none

  private
  public read_column

  interface read_column
     ! This generic procedure reads a column of values in an external
     ! file. The records of the file shoud be formatted. If the
     ! argument "last" is 0 or is absent then the procedure reads to
     ! the last line in the file. The difference between the specific
     ! procedures is the type of argument "a".
     module procedure read_column_real, read_column_char
  end interface
  
contains

  subroutine read_column_real(file, a, first, last)

    use new_unit_m, only: new_unit

    character(len=*), intent(in):: file
    real, pointer:: a(:)
    integer, intent(in), optional:: first ! (first line to read)
    integer, intent(in), optional:: last ! (last line to read)

    ! Variables local to the subprogram:
    integer unit ! external file unit
    integer first_not_opt ! first line to read, local variable
    integer last_not_opt ! last line to read, local variable

    !------------------------------------------------------

    call new_unit(unit)
    open(unit, file=file, status='old', action='read', position='rewind')
    call prep_file(unit, first, last, first_not_opt, last_not_opt)
    allocate(a(last_not_opt - first_not_opt + 1))
    read(unit, fmt=*) a
    close(unit)

  end subroutine read_column_real

  !***********************************************************

  subroutine read_column_char(file, a, first, last)

    use new_unit_m, only: new_unit

    character(len=*), intent(in):: file
    character(len=*), pointer:: a(:)
    integer, intent(in), optional:: first ! (first line to read)
    integer, intent(in), optional:: last ! (last line to read)

    ! Variables local to the subprogram:
    integer unit
    integer first_not_opt ! first line to read, local variable
    integer last_not_opt ! last line to read, local variable

    !------------------------------------------------------

    call new_unit(unit)
    open(unit, file=file, status='old', action='read', position='rewind')
    call prep_file(unit, first, last, first_not_opt, last_not_opt)
    allocate(a(last_not_opt - first_not_opt + 1))
    read(unit, fmt=*) a
    close(unit)

  end subroutine read_column_char

  !***********************************************************

  subroutine prep_file(unit, first, last, first_not_opt, last_not_opt)

    ! This subroutine is used by the various versions of "read_column".
    ! It fills non-optional arguments: first and last line which will
    ! actually be read, taking information from the file itself if necessary.
    ! It also positions the input file on the first line to read.

    use opt_merge_m, only: opt_merge

    integer, intent(in):: unit ! logical unit for input file
    integer, intent(in), optional:: first ! (first line to read)
    integer, intent(in), optional:: last ! (last line to read)
    integer, intent(out):: first_not_opt ! (first line to read, not optional)
    integer, intent(out):: last_not_opt ! (last line to read, not optional)

    ! Variables local to the subprogram:
    integer iostat, i

    !------------------------------------------------------

    first_not_opt = opt_merge(first, 1)
    last_not_opt = opt_merge(last, 0)

    if (last_not_opt == 0) then
       ! Count the number of lines in the file:
       i = 0
       do
          read(unit, fmt=*, iostat=iostat)
          if (iostat /= 0) exit
          i = i + 1
       end do
       last_not_opt = i
       if (last_not_opt == 0) stop 'Empty file.'

       rewind(unit)
    end if

    ! Go to first line to read:
    do i = 1, first_not_opt - 1
       read(unit, fmt=*)
    end do

  end subroutine prep_file

end module read_column_m
