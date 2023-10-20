module get_command_arg_dyn_m

    use, intrinsic:: iso_fortran_env, only: error_unit

  implicit none

  interface get_command_arg_dyn
     ! This generic procedure is a wrapper for the intrinsic procedure
     ! get_command_argument. The advantages over a direct call to
     ! get_command_argument are:

     ! - The arg_val character argument is automatically allocated to the
     ! correct length.

     ! - You do not have to check the return status:
     ! get_command_arg_dyn aborts if there is a problem.

     ! - You can directly read an integer value from an argument.

     module procedure get_command_arg_dyn_char, get_command_arg_dyn_int
  end interface get_command_arg_dyn

  ! Note: no need to call command_argument_count first, because
  ! there is probably no other reason for failure of the first call
  ! to get_command_argument below than "number" being greater than
  ! number of arguments.

  private
  public get_command_arg_dyn

contains

  subroutine get_command_arg_dyn_char(number, arg_val, message)

    integer, intent(in):: number
    character(len = :), allocatable, intent(out):: arg_val
    character(len = *), intent(in), optional:: message

    ! Local:
    integer length, status

    !---------------------------------------------------------------------

    call get_command_argument(number, length = length, status = status)

    if (status /= 0) then
       write(error_unit, fmt = *) "get_command_arg_dyn_char: number = ", number
       write(error_unit, fmt = *) "get_command_arg_dyn_char: status = ", status
       if (present(message)) write(error_unit, fmt = *) message
       stop 1
    end if

    allocate(character(len = length):: arg_val)
    call get_command_argument(number, arg_val)

  end subroutine get_command_arg_dyn_char

  !************************************************************************

  subroutine get_command_arg_dyn_int(number, arg_val, message)

    integer, intent(in):: number
    integer, intent(out):: arg_val
    character(len = *), intent(in), optional:: message

    ! Local:
    integer length, status
    character(len = :), allocatable:: arg_val_char

    !---------------------------------------------------------------------

    call get_command_argument(number, length = length, status = status)

    if (status /= 0) then
       write(error_unit, fmt = *) "get_command_arg_dyn_int: number = ", number
       write(error_unit, fmt = *) "get_command_arg_dyn_int: status = ", status
       if (present(message)) write(error_unit, fmt = *) message
       stop 1
    end if

    allocate(character(len = length):: arg_val_char)
    call get_command_argument(number, arg_val_char)
    read(unit = arg_val_char, fmt = *) arg_val

  end subroutine get_command_arg_dyn_int

end module get_command_arg_dyn_m
