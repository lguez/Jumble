program test_squeeze

  use jumble, only: squeeze

  implicit none

  character(len = 100) line

  !-----------------------------------------------------------------------

  print *, "Enter a character string, with quotes:"
  read *, line
  write(unit = *, fmt = "(3a)") '"', squeeze(line, " "), '"'

end program test_squeeze
