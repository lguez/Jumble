program test_readline

  use, intrinsic:: iso_fortran_env

  use jumble, only: readline, get_command_arg_dyn, new_unit

  implicit none

  character(len = :), allocatable :: line, fname
  integer unit, iostat

  !--------------------------------------------------------------

  call get_command_arg_dyn(1, fname, "Required argument: FILE")

  if (fname == "-") then
     unit = INPUT_UNIT
  else
     call new_unit(unit)
     open(unit, file = fname, status = "old", action = "read", &
          position = "rewind")
  end if

  do
     call readline(unit, line, iostat)
     if (iostat /= 0) exit
     write(unit = *, fmt = "(3a)") '"', line, '"'
  enddo

  if (fname /= "-") close(unit)

end program test_readline
