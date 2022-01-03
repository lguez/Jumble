MODULE assert_eq_m

  implicit none

  INTERFACE assert_eq
     ! Exit with error message if integer arguments not all
     ! equal. Embedding program dies gracefully with an error message
     ! if any of the integer arguments are not equal to the
     ! first. Otherwise, return the value of the first
     ! argument. Typical use is for enforcing equality on the sizes of
     ! arrays passed to a subprogram. assert_eq implements and
     ! overloads forms with 1, 2, 3, and 4 integer arguments, plus a
     ! form with a vector integer argument,
     MODULE PROCEDURE assert_eq2,assert_eq3,assert_eq4,assert_eqn
  END INTERFACE

  private assert_eq2,assert_eq3,assert_eq4,assert_eqn

CONTAINS

  FUNCTION assert_eq2(n1,n2,string)
    CHARACTER(LEN=*), INTENT(IN) :: string
    INTEGER, INTENT(IN) :: n1,n2
    INTEGER  assert_eq2
    if (n1 == n2) then
       assert_eq2=n1
    else
       write (*,*) 'nrerror: an assert_eq failed with this tag: ', &
            string
       print *, 'program terminated by assert_eq2'
       stop 1
    end if
  END FUNCTION assert_eq2

  FUNCTION assert_eq3(n1,n2,n3,string)
    CHARACTER(LEN=*), INTENT(IN) :: string
    INTEGER, INTENT(IN) :: n1,n2,n3
    INTEGER  assert_eq3
    if (n1 == n2 .and. n2 == n3) then
       assert_eq3=n1
    else
       write (*,*) 'nrerror: an assert_eq failed with this tag: ', &
            string
       print *, 'program terminated by assert_eq3'
       stop 1
    end if
  END FUNCTION assert_eq3

  FUNCTION assert_eq4(n1,n2,n3,n4,string)
    CHARACTER(LEN=*), INTENT(IN) :: string
    INTEGER, INTENT(IN) :: n1,n2,n3,n4
    INTEGER  assert_eq4
    if (n1 == n2 .and. n2 == n3 .and. n3 == n4) then
       assert_eq4=n1
    else
       write (*,*) 'nrerror: an assert_eq failed with this tag: ', &
            string
       print *, 'program terminated by assert_eq4'
       stop 1
    end if
  END FUNCTION assert_eq4

  FUNCTION assert_eqn(nn,string)
    CHARACTER(LEN=*), INTENT(IN) :: string
    INTEGER, DIMENSION(:), INTENT(IN) :: nn
    INTEGER  assert_eqn
    if (all(nn(2:) == nn(1))) then
       assert_eqn=nn(1)
    else
       write (*,*) 'nrerror: an assert_eq failed with this tag: ', &
            string
       print *, 'program terminated by assert_eqn'
       stop 1
    end if
  END FUNCTION assert_eqn

END MODULE assert_eq_m
