module get_command_arg_dyn_m

  implicit none

contains

  subroutine get_command_arg_dyn(number, arg_val, message)

    ! This procedure is a wrapper for the intrinsic procedure
    ! get_command_argument. The advantage over a direct call to
    ! get_command_argument is that the arg_val argument is
    ! automatically allocated to the correct length. Another
    ! difference with a direct call to get_command_argument is that
    ! you do not have to check the return status: get_command_arg_dyn
    ! aborts if there is a problem.

    ! Note: no need to call command_argument_count first, because
    ! there is probably no other reason for failure of the first call
    ! to get_command_argument below than "number" being greater than
    ! number of arguments.

    use, intrinsic:: iso_fortran_env
    
    integer, intent(in):: number
    character(len = :), allocatable, intent(out):: arg_val
    character(len = *), intent(in), optional:: message

    ! Local:
    integer length, status

    !---------------------------------------------------------------------

    call get_command_argument(number, length = length, status = status)

    if (status /= 0) then
       write(error_unit, fmt = *) "get_command_arg_dyn: number = ", number
       write(error_unit, fmt = *) "get_command_arg_dyn: status = ", status
       if (present(message)) write(error_unit, fmt = *) message
       stop 1
    end if

    allocate(character(len = length):: arg_val)
    call get_command_argument(number, arg_val)

  end subroutine get_command_arg_dyn

end module get_command_arg_dyn_m
