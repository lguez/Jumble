module readline_m

  implicit none

contains

  subroutine readline(unit, line, iostat, iomsg)

    ! This subroutine reads one line from an external file, from the
    ! current position, not necessarily the first character of the
    ! record. The line can have an arbitrary length. The file should
    ! be connected for sequential access. The records of the file
    ! shoud be formatted.

    ! On return, iostat = 0 for success. If success, the position is
    ! after the record read. If an error or end-of-file occurs, an
    ! explanatory message less than 100 characters long is assigned to
    ! iomsg, otherwise this variable is unchanged.

    use, intrinsic:: iso_fortran_env

    integer, intent(in):: unit
    character(len = :), allocatable, intent(out):: line
    integer, intent(out), optional:: iostat
    character(len = *), intent(inout), optional:: iomsg

    ! Local:
    integer, parameter :: buflen = 100
    character(len = buflen) buffer
    integer isize, total_size, iostat_local
    logical hit_eor
    character(len = 100) iomsg_local

    !---------------------------------------------------------

    line = ''
    total_size = 0

    do
       read(unit, iostat = iostat_local, fmt = '(a)', advance = 'no', &
            size = isize, iomsg = iomsg_local) buffer
       hit_eor = is_iostat_eor(iostat_local)

       if (iostat_local == 0 .or. hit_eor) then
          if (isize > 0) then
             line = line // buffer(:isize)
             total_size = total_size + isize
          end if
       end if

       if (iostat_local /= 0) exit
    end do

    if (hit_eor) then
       line = line(:total_size)

       if (present(iostat)) iostat = 0
       ! (We choose 0 rather than iostat_local, which has the
       ! end-of-record code.)
    else
       if (present(iostat)) then
          iostat = iostat_local
          if (present(iomsg)) iomsg = iomsg_local
       else
          write(error_unit, fmt = *) "readline: ", trim(iomsg_local)
          stop 1
       end if
    end if

  end subroutine readline

end module readline_m
