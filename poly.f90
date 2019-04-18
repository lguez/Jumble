MODULE poly_m

  IMPLICIT NONE

  INTEGER, PARAMETER, private :: NPAR_POLY=8

  INTERFACE poly

     ! Evaluate a polynomial P (x) for one or more values x, with
     ! optional mask.

     ! Returns a scalar value or array with the same type and shape as
     ! x, containing the result of evaluating the polynomial with
     ! one-dimensional coefficient vector coeffs on each component of
     ! x. The optional argument mask, if present, has the same shape
     ! as x, and suppresses evaluation of the polynomial where its
     ! components are .false..

     MODULE PROCEDURE poly_rr,poly_rrv,poly_dd,poly_ddv,&
          poly_rc,poly_cc,poly_msk_rrv,poly_msk_ddv
  END INTERFACE

  private poly_rr,poly_rrv,poly_dd,poly_ddv,&
       poly_rc,poly_cc,poly_msk_rrv,poly_msk_ddv

CONTAINS

  FUNCTION poly_rr(x,coeffs)
    REAL, INTENT(IN) :: x
    REAL, DIMENSION(:), INTENT(IN) :: coeffs
    REAL :: poly_rr
    REAL :: pow
    REAL, DIMENSION(:), ALLOCATABLE :: vec
    INTEGER :: i,n,nn
    n=size(coeffs)
    if (n <= 0) then
       poly_rr=0.0
    else if (n < NPAR_POLY) then
       poly_rr=coeffs(n)
       do i=n-1,1,-1
          poly_rr=x*poly_rr+coeffs(i)
       end do
    else
       allocate(vec(n+1))
       pow=x
       vec(1:n)=coeffs
       do
          vec(n+1)=0.0
          nn=ishft(n+1,-1)
          vec(1:nn)=vec(1:n:2)+pow*vec(2:n+1:2)
          if (nn == 1) exit
          pow=pow*pow
          n=nn
       end do
       poly_rr=vec(1)
       deallocate(vec)
    end if
  END FUNCTION poly_rr

  FUNCTION poly_dd(x,coeffs)
    DOUBLE PRECISION, INTENT(IN) :: x
    DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: coeffs
    DOUBLE PRECISION :: poly_dd
    DOUBLE PRECISION :: pow
    DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: vec
    INTEGER :: i,n,nn
    n=size(coeffs)
    if (n <= 0) then
       poly_dd=0.0d0
    else if (n < NPAR_POLY) then
       poly_dd=coeffs(n)
       do i=n-1,1,-1
          poly_dd=x*poly_dd+coeffs(i)
       end do
    else
       allocate(vec(n+1))
       pow=x
       vec(1:n)=coeffs
       do
          vec(n+1)=0.0d0
          nn=ishft(n+1,-1)
          vec(1:nn)=vec(1:n:2)+pow*vec(2:n+1:2)
          if (nn == 1) exit
          pow=pow*pow
          n=nn
       end do
       poly_dd=vec(1)
       deallocate(vec)
    end if
  END FUNCTION poly_dd

  FUNCTION poly_rc(x,coeffs)
    COMPLEX, INTENT(IN) :: x
    REAL, DIMENSION(:), INTENT(IN) :: coeffs
    COMPLEX :: poly_rc
    COMPLEX :: pow
    COMPLEX, DIMENSION(:), ALLOCATABLE :: vec
    INTEGER :: i,n,nn
    n=size(coeffs)
    if (n <= 0) then
       poly_rc=0.0
    else if (n < NPAR_POLY) then
       poly_rc=coeffs(n)
       do i=n-1,1,-1
          poly_rc=x*poly_rc+coeffs(i)
       end do
    else
       allocate(vec(n+1))
       pow=x
       vec(1:n)=coeffs
       do
          vec(n+1)=0.0
          nn=ishft(n+1,-1)
          vec(1:nn)=vec(1:n:2)+pow*vec(2:n+1:2)
          if (nn == 1) exit
          pow=pow*pow
          n=nn
       end do
       poly_rc=vec(1)
       deallocate(vec)
    end if
  END FUNCTION poly_rc

  FUNCTION poly_cc(x,coeffs)
    COMPLEX, INTENT(IN) :: x
    COMPLEX, DIMENSION(:), INTENT(IN) :: coeffs
    COMPLEX :: poly_cc
    COMPLEX :: pow
    COMPLEX, DIMENSION(:), ALLOCATABLE :: vec
    INTEGER :: i,n,nn
    n=size(coeffs)
    if (n <= 0) then
       poly_cc=0.0
    else if (n < NPAR_POLY) then
       poly_cc=coeffs(n)
       do i=n-1,1,-1
          poly_cc=x*poly_cc+coeffs(i)
       end do
    else
       allocate(vec(n+1))
       pow=x
       vec(1:n)=coeffs
       do
          vec(n+1)=0.0
          nn=ishft(n+1,-1)
          vec(1:nn)=vec(1:n:2)+pow*vec(2:n+1:2)
          if (nn == 1) exit
          pow=pow*pow
          n=nn
       end do
       poly_cc=vec(1)
       deallocate(vec)
    end if
  END FUNCTION poly_cc

  FUNCTION poly_rrv(x,coeffs)
    REAL, DIMENSION(:), INTENT(IN) :: coeffs,x
    REAL, DIMENSION(size(x)) :: poly_rrv
    INTEGER :: i,n,m
    m=size(coeffs)
    n=size(x)
    if (m <= 0) then
       poly_rrv=0.0
    else if (m < n .or. m < NPAR_POLY) then
       poly_rrv=coeffs(m)
       do i=m-1,1,-1
          poly_rrv=x*poly_rrv+coeffs(i)
       end do
    else
       do i=1,n
          poly_rrv(i)=poly_rr(x(i),coeffs)
       end do
    end if
  END FUNCTION poly_rrv

  FUNCTION poly_ddv(x,coeffs)
    DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: coeffs,x
    DOUBLE PRECISION, DIMENSION(size(x)) :: poly_ddv
    INTEGER :: i,n,m
    m=size(coeffs)
    n=size(x)
    if (m <= 0) then
       poly_ddv=0.0d0
    else if (m < n .or. m < NPAR_POLY) then
       poly_ddv=coeffs(m)
       do i=m-1,1,-1
          poly_ddv=x*poly_ddv+coeffs(i)
       end do
    else
       do i=1,n
          poly_ddv(i)=poly_dd(x(i),coeffs)
       end do
    end if
  END FUNCTION poly_ddv

  FUNCTION poly_msk_rrv(x,coeffs,mask)
    REAL, DIMENSION(:), INTENT(IN) :: coeffs,x
    LOGICAL, DIMENSION(:), INTENT(IN) :: mask
    REAL, DIMENSION(size(x)) :: poly_msk_rrv
    poly_msk_rrv=unpack(poly_rrv(pack(x,mask),coeffs),mask,0.0)
  END FUNCTION poly_msk_rrv

  FUNCTION poly_msk_ddv(x,coeffs,mask)
    DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: coeffs,x
    LOGICAL, DIMENSION(:), INTENT(IN) :: mask
    DOUBLE PRECISION, DIMENSION(size(x)) :: poly_msk_ddv
    poly_msk_ddv=unpack(poly_ddv(pack(x,mask),coeffs),mask,0.0d0)
  END FUNCTION poly_msk_ddv

END MODULE poly_m
