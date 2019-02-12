module GREG2JD_m

  ! Programmer: David G. Simpson
  ! NASA Goddard Space Flight Center
  ! Greenbelt, Maryland 20771
  ! Date: November 20, 2001
  ! Version: 1.00b (October 25, 2004)

  IMPLICIT NONE

  private GREGORIAN

contains

  subroutine GREG2JD(y, m, d, hour, JD, GREGORIAN_FLAG)

    ! This procedure converts a date on the Gregorian or Julian calendars
    ! to a Julian date.

    INTEGER, intent(in):: Y ! year
    INTEGER, intent(in):: M ! month (1-12)
    integer, intent(in):: D
    real, intent(in):: hour ! with fractional part

    DOUBLE PRECISION, intent(out):: JD ! Julian date
    LOGICAL, intent(out), optional:: GREGORIAN_FLAG ! Gregorian date

    ! Variables local to the procedure:
    INTEGER A, B, yy, mm
    logical gf

    !-------------------------------------------------------------------------

    ! Test for Gregorian calendar:
    gf = GREGORIAN(Y, M, D)

    IF (M <= 2) THEN
       Yy = Y - 1
       Mm = M + 12
    else
       yy = y
       mm = m
    END IF

    IF (gf) THEN
       ! Gregorian calendar
       A = YY / 100
       B = 2 - A + A / 4
    ELSE
       ! Julian calendar
       B = 0
    END IF

    JD = INT(365.25D0 * (YY + 4716)) + INT(30.6001D0 * (MM + 1)) + D + B &
         - 1524.5D0 + hour / 24d0
    if (present(GREGORIAN_FLAG)) GREGORIAN_FLAG = gf

  END subroutine GREG2JD

  !*******************************************************

  LOGICAL FUNCTION GREGORIAN(YEAR, MONTH, DAY)

    ! This function determines whether a given date is in the
    ! Gregorian calendar (return value of .TRUE.) or on the Julian calendar
    ! (return value of .FALSE.).

    INTEGER, INTENT(IN):: YEAR ! input year
    INTEGER, INTENT(IN):: MONTH ! input month
    INTEGER, INTENT(IN):: DAY ! input day of month

    ! Variables local to the procedure:

    ! Julian stop/Gregorian start dates:
    INTEGER, PARAMETER:: YEAR_J = 1582 ! year of end of Julian calendar
    INTEGER, PARAMETER:: MONTH_J = 10 ! month of end of Julian calendar
    INTEGER, PARAMETER:: DAY_J = 4 ! day of end of Julian calendar
    INTEGER, PARAMETER:: YEAR_G = 1582 ! year of start of Gregorian calendar
    INTEGER, PARAMETER:: MONTH_G = 10 ! month of start of Gregorian calendar
    INTEGER, PARAMETER:: DAY_G = 15 ! day of start of Gregorian calendar

    INTEGER CALTYPE ! 0=unknown, 1=Julian, 2=Gregorian

    !---------------------------------------------------------------

    CALTYPE = 0

    IF (YEAR < YEAR_J) THEN
       ! year before end of Julian calendar
       CALTYPE = 1 ! then this is a Julian date
    ELSE IF (YEAR == YEAR_J) THEN
       ! this is the last year of the Julian cal
       IF (MONTH < MONTH_J) THEN
          ! this is before the ending month
          CALTYPE = 1 ! this is a Julian date
       ELSE IF (MONTH == MONTH_J) THEN
          ! this is the ending month
          IF (DAY <= DAY_J) THEN
             ! this is before/at the ending date
             CALTYPE = 1 ! this is a Julian date
          END IF
       END IF
    END IF

    IF (YEAR > YEAR_G) THEN
       ! year after start of Gregorian calendar
       CALTYPE = 2 ! this is a Gregorian date
    ELSE IF (YEAR == YEAR_G) THEN
       ! this is the first year of the Greg. cal
       IF (MONTH > MONTH_G) THEN
          ! this is after the starting month
          CALTYPE = 2 ! this is a Gregorian date
       ELSE IF (MONTH == MONTH_G) THEN
          ! this is the starting month
          IF (DAY >= DAY_G) THEN
             ! this is at/after the starting date
             CALTYPE = 2 ! this is a Gregorian date
          END IF
       END IF
    END IF

    ! Check calendar type:
    SELECT CASE (CALTYPE)
    CASE (0)
       ! unknown, we have an invalid date
       WRITE (UNIT=*, FMT='(A)') ' No such date.'
       STOP 1
    CASE (1)
       ! Julian date
       gregorian = .FALSE.
    CASE (2)
       ! Gregorian date
       gregorian = .TRUE.
    END SELECT

  END FUNCTION GREGORIAN

end module GREG2JD_m
