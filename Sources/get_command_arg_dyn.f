module get_command_arg_dyn_m

  implicit none

contains

  subroutine get_command_arg_dyn(number, value)

    integer, intent(in):: number
    character(len = :), allocatable, intent(out):: value

    ! Local:
    integer length, status

    !---------------------------------------------------------------------

    call get_command_argument(number, length = length, status = status)

    if (status /= 0) then
       print *, "get_command_arg_dyn: number = ", number
       print *, "get_command_arg_dyn: status = ", status
       stop 1
    end if

    allocate(character(len = length):: value)
    call get_command_argument(number, value)

  end subroutine get_command_arg_dyn

end module get_command_arg_dyn_m
