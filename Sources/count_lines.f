module count_lines_m

  implicit none

contains

  subroutine count_lines(unit, n)

    ! This subroutine counts the number of lines in an external file,
    ! from the current position, not necessarily the first record of the
    ! file.
    ! On return, the position is at the end of the file.
    ! The file should be connected for sequential access.
    ! The records of the file shoud be formatted.

    integer, intent(in):: unit ! external file unit
    integer, intent(out):: n ! number of lines

    ! Variable local to the procedure:
    integer iostat

    !-------------------------------

    n = 0
    do
       read(unit, fmt=*, iostat=iostat)
       if (iostat /= 0) exit
       n = n + 1
    end do

  end subroutine count_lines

end module count_lines_m
