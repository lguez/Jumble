MODULE assert_m

  implicit none

  INTERFACE assert
     MODULE PROCEDURE assert1,assert2,assert3,assert4,assert_v
  END INTERFACE

  private assert1,assert2,assert3,assert4,assert_v

CONTAINS

  SUBROUTINE assert1(n1,string)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL, INTENT(IN) :: n1
    if (.not. n1) then
       print *, 'An assertion failed with this tag: ' // string
       print *, 'program terminated by assert1'
       stop 1
    end if
  END SUBROUTINE assert1

  !****************************

  SUBROUTINE assert2(n1,n2,string)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL, INTENT(IN) :: n1,n2
    if (.not. (n1 .and. n2)) then
       print *, 'An assertion failed with this tag: ' // string
       print *, 'program terminated by assert2'
       stop 1
    end if
  END SUBROUTINE assert2

  !****************************

  SUBROUTINE assert3(n1,n2,n3,string)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL, INTENT(IN) :: n1,n2,n3
    if (.not. (n1 .and. n2 .and. n3)) then
       print *, 'An assertion failed with this tag: ' // string
       print *, 'program terminated by assert3'
       stop 1
    end if
  END SUBROUTINE assert3

  !****************************

  SUBROUTINE assert4(n1,n2,n3,n4,string)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL, INTENT(IN) :: n1,n2,n3,n4
    if (.not. (n1 .and. n2 .and. n3 .and. n4)) then
       print *, 'An assertion failed with this tag: ' // string
       print *, 'program terminated by assert4'
       stop 1
    end if
  END SUBROUTINE assert4

  !****************************

  SUBROUTINE assert_v(n,string)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL, DIMENSION(:), INTENT(IN) :: n
    if (.not. all(n)) then
       print *, 'An assertion failed with this tag: ' // string
       print *, 'program terminated by assert_v'
       stop 1
    end if
  END SUBROUTINE assert_v

END MODULE assert_m
