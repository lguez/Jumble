module get_command_arg_dyn_m

  implicit none

contains

  subroutine get_command_arg_dyn(number, arg_val, message)

    integer, intent(in):: number
    character(len = :), allocatable, intent(out):: arg_val
    character(len = *), intent(in), optional:: message

    ! Local:
    integer length, status

    !---------------------------------------------------------------------

    call get_command_argument(number, length = length, status = status)

    if (status /= 0) then
       print *, "get_command_arg_dyn: number = ", number
       print *, "get_command_arg_dyn: status = ", status
       if (present(message)) print *, message
       stop 1
    end if

    allocate(character(len = length):: arg_val)
    call get_command_argument(number, arg_val)

  end subroutine get_command_arg_dyn

end module get_command_arg_dyn_m
