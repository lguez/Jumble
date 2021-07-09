PROGRAM test_GREG2JD

  use jumble, only: greg2jd

  IMPLICIT NONE

  integer D ! day of month
  DOUBLE PRECISION JD ! Julian day
  INTEGER MONTH ! month (1-12)
  INTEGER Y ! year
  LOGICAL GREGORIAN_FLAG ! .TRUE. for Gregorian date, .FALSE. for Julian
  integer h, mn, s

  !---------------------------------------------------------------------------

  write(unit=*, fmt="(a)", advance="no") "year, month, day (integer values)? "
  read *, y, month, d
  write(unit=*, fmt="(a)", advance="no") "h, mn, s (integer values)? "
  read *, h, mn, s
  call GREG2JD(y, month, d, h + (mn + s / 60.) / 60., JD, GREGORIAN_FLAG)
  print *
  IF (.NOT. GREGORIAN_FLAG) print *, 'Julian calendar'
  print '(A, F15.6)', ' Julian date = ', JD

END PROGRAM TEST_GREG2JD
