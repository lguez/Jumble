module prep_file_m

  implicit none

  ! This module is not used in the grouping module jumble, since
  ! prep_file is only useful to csvread.

contains

  subroutine prep_file(unit, first_r, first_c, last_r, last_c, f_r_not_opt, &
       f_c_not_opt, l_r_not_opt, l_c_not_opt)

    ! This subroutine is used by "csvread". It fills non-optional
    ! arguments: first and last row, first and last column which will
    ! actually be read, taking information from the file itself if
    ! necessary. It also positions the input file on the first row to
    ! read.

    ! Does not work with several adjacent commas.

    use count_lines_m, only: count_lines
    use opt_merge_m, only: opt_merge

    integer, intent(in):: unit ! logical unit for input file
    integer, intent(in), optional:: first_r ! (first row to read)
    integer, intent(in), optional:: first_c ! (first column to read)
    integer, intent(in), optional:: last_r ! (last row to read)
    integer, intent(in), optional:: last_c ! (last column to read)
    integer, intent(out):: f_r_not_opt ! (first row to read, not optional)
    integer, intent(out):: f_c_not_opt ! (first column to read, not optional)
    integer, intent(out):: l_r_not_opt ! (last row to read, not optional)
    integer, intent(out):: l_c_not_opt ! (last column to read, not optional)

    ! Local:
    integer iostat, i
    character c
    logical prev_value ! previous character was part of a value
    logical curr_value ! current character is part of a value

    !------------------------------------------------------

    f_r_not_opt = opt_merge(first_r, 1)
    f_c_not_opt = opt_merge(first_c, 1)
    l_r_not_opt = opt_merge(last_r, 0)
    l_c_not_opt = opt_merge(last_c, 0)

    if (l_r_not_opt == 0) then
       call count_lines(unit, l_r_not_opt)
       if (l_r_not_opt == 0) stop 'Empty file.'
       rewind(unit)
    end if

    ! Go to first row to read:
    do i = 1, f_r_not_opt - 1
       read(unit, fmt=*)
    end do

    if (l_c_not_opt == 0) then
       ! Count the number of values per line:
       l_c_not_opt = 0
       curr_value = .false.
       do
          read(unit, fmt='(a)', advance='no', iostat=iostat) c
          if (iostat /= 0) exit
          prev_value = curr_value
          curr_value = c /= " " .and. c /= ","
          if (curr_value .and. .not. prev_value) l_c_not_opt = l_c_not_opt + 1
       end do

       backspace(unit)
    end if

    print *, 'Reading column(s) ', f_c_not_opt, ':', l_c_not_opt, &
         ', row(s) ', f_r_not_opt, ':', l_r_not_opt

  end subroutine prep_file

end module prep_file_m
