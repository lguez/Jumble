MODULE assert_m

  use, intrinsic:: iso_fortran_env, only: error_unit

  implicit none

  INTERFACE assert
     ! Exit with error message if any assertion is false. Embedding
     ! program dies gracefully with an error message if any of the
     ! logical arguments are false. Typical use is with logical
     ! expressions as the actual arguments. assert implements and
     ! overloads forms with 1, 2, 3, and 4 logical arguments, plus a
     ! form with a vector logical argument
     MODULE PROCEDURE assert1,assert2,assert3,assert4,assert_v
  END INTERFACE assert

  private
  public assert

CONTAINS

  SUBROUTINE assert1(n1,string)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL, INTENT(IN) :: n1
    if (.not. n1) then
       write(error_unit, fmt = *) 'An assertion failed with this tag: ' &
            // string
       write(error_unit, fmt = *) 'program terminated by assert1'
       stop 1
    end if
  END SUBROUTINE assert1

  !****************************

  SUBROUTINE assert2(n1,n2,string)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL, INTENT(IN) :: n1,n2
    if (.not. (n1 .and. n2)) then
       write(error_unit, fmt = *) 'An assertion failed with this tag: ' &
            // string
       write(error_unit, fmt = *) 'program terminated by assert2'
       stop 1
    end if
  END SUBROUTINE assert2

  !****************************

  SUBROUTINE assert3(n1,n2,n3,string)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL, INTENT(IN) :: n1,n2,n3
    if (.not. (n1 .and. n2 .and. n3)) then
       write(error_unit, fmt = *) 'An assertion failed with this tag: ' &
            // string
       write(error_unit, fmt = *) 'program terminated by assert3'
       stop 1
    end if
  END SUBROUTINE assert3

  !****************************

  SUBROUTINE assert4(n1,n2,n3,n4,string)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL, INTENT(IN) :: n1,n2,n3,n4
    if (.not. (n1 .and. n2 .and. n3 .and. n4)) then
       write(error_unit, fmt = *) 'An assertion failed with this tag: ' &
            // string
       write(error_unit, fmt = *) 'program terminated by assert4'
       stop 1
    end if
  END SUBROUTINE assert4

  !****************************

  SUBROUTINE assert_v(n,string)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL, DIMENSION(:), INTENT(IN) :: n
    if (.not. all(n)) then
       write(error_unit, fmt = *) 'An assertion failed with this tag: ' &
            // string
       write(error_unit, fmt = *) 'program terminated by assert_v'
       stop 1
    end if
  END SUBROUTINE assert_v

END MODULE assert_m
