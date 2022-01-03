MODULE arth_m

  IMPLICIT NONE

  INTEGER, PARAMETER, private:: NPAR_ARTH=16, NPAR2_ARTH=8

  INTERFACE arth
     ! Returns an arithmetic progression, given a first term "first", an
     ! increment and a number of terms "n" (including "first").

     MODULE PROCEDURE arth_r, arth_d, arth_i
     ! The difference between the procedures is the kind and type of
     ! arguments first and increment and of function result.
  END INTERFACE

  private arth_r, arth_d, arth_i

CONTAINS

  pure FUNCTION arth_r(first,increment,n)

    REAL, INTENT(IN) :: first,increment
    INTEGER, INTENT(IN) :: n
    REAL arth_r(n)

    ! Local:
    INTEGER :: k,k2
    REAL :: temp

    !---------------------------------------

    if (n > 0) arth_r(1)=first
    if (n <= NPAR_ARTH) then
       do k=2,n
          arth_r(k)=arth_r(k-1)+increment
       end do
    else
       do k=2,NPAR2_ARTH
          arth_r(k)=arth_r(k-1)+increment
       end do
       temp=increment*NPAR2_ARTH
       k=NPAR2_ARTH
       do
          if (k >= n) exit
          k2=k+k
          arth_r(k+1:min(k2,n)) = temp + arth_r(1:min(k,n-k))
          temp=temp+temp
          k=k2
       end do
    end if

  END FUNCTION arth_r

  !*************************************

  pure FUNCTION arth_d(first,increment,n)

    DOUBLE PRECISION, INTENT(IN) :: first,increment
    INTEGER, INTENT(IN) :: n
    DOUBLE PRECISION arth_d(n)

    ! Local:
    INTEGER :: k,k2
    DOUBLE PRECISION :: temp

    !---------------------------------------

    if (n > 0) arth_d(1)=first
    if (n <= NPAR_ARTH) then
       do k=2,n
          arth_d(k)=arth_d(k-1)+increment
       end do
    else
       do k=2,NPAR2_ARTH
          arth_d(k)=arth_d(k-1)+increment
       end do
       temp=increment*NPAR2_ARTH
       k=NPAR2_ARTH
       do
          if (k >= n) exit
          k2=k+k
          arth_d(k+1:min(k2,n)) = temp + arth_d(1:min(k,n-k))
          temp=temp+temp
          k=k2
       end do
    end if

  END FUNCTION arth_d

  !*************************************

  pure FUNCTION arth_i(first,increment,n)

    INTEGER, INTENT(IN) :: first,increment,n
    INTEGER arth_i(n)

    ! Local:
    INTEGER :: k,k2,temp

    !---------------------------------------

    if (n > 0) arth_i(1)=first
    if (n <= NPAR_ARTH) then
       do k=2,n
          arth_i(k)=arth_i(k-1)+increment
       end do
    else
       do k=2,NPAR2_ARTH
          arth_i(k)=arth_i(k-1)+increment
       end do
       temp=increment*NPAR2_ARTH
       k=NPAR2_ARTH
       do
          if (k >= n) exit
          k2=k+k
          arth_i(k+1:min(k2,n))=temp+arth_i(1:min(k,n-k))
          temp=temp+temp
          k=k2
       end do
    end if

  END FUNCTION arth_i

END MODULE arth_m
