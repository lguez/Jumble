MODULE poly_term_m

  IMPLICIT NONE

  INTEGER, PARAMETER, private:: NPAR_POLYTERM=8

  INTERFACE poly_term
     MODULE PROCEDURE poly_term_rr, poly_term_cc
     ! Tabulate cumulants of a polynomial.

     ! Returns an array of type and size the same as the
     ! one-dimensional array a, containing the partial cumulants of
     ! the polynomial with coefficients a (arranged from highest-order
     ! to lowest-order coefficients, n.b.) evaluated at x. This is
     ! equivalent to synthetic division, and can be parallelized. See
     ! § 22.3. Note that the order of arguments is reversed in poly
     ! and poly_term —each routine returns a value with the size and
     ! shape of the first argument, the usual Fortran 90 convention.
  END INTERFACE

  private poly_term_rr,poly_term_cc

CONTAINS

  RECURSIVE FUNCTION poly_term_rr(a,b) RESULT(u)
    REAL, DIMENSION(:), INTENT(IN) :: a
    REAL, INTENT(IN) :: b
    REAL, DIMENSION(size(a)) :: u
    INTEGER n,j
    n=size(a)
    if (n <= 0) RETURN
    u(1)=a(1)
    if (n < NPAR_POLYTERM) then
       do j=2,n
          u(j)=a(j)+b*u(j-1)
       end do
    else
       u(2:n:2)=poly_term_rr(a(2:n:2)+a(1:n-1:2)*b,b*b)
       u(3:n:2)=a(3:n:2)+b*u(2:n-1:2)
    end if
  END FUNCTION poly_term_rr

  RECURSIVE FUNCTION poly_term_cc(a,b) RESULT(u)
    COMPLEX, DIMENSION(:), INTENT(IN) :: a
    COMPLEX, INTENT(IN) :: b
    COMPLEX, DIMENSION(size(a)) :: u
    INTEGER n,j
    n=size(a)
    if (n <= 0) RETURN
    u(1)=a(1)
    if (n < NPAR_POLYTERM) then
       do j=2,n
          u(j)=a(j)+b*u(j-1)
       end do
    else
       u(2:n:2)=poly_term_cc(a(2:n:2)+a(1:n-1:2)*b,b*b)
       u(3:n:2)=a(3:n:2)+b*u(2:n-1:2)
    end if
  END FUNCTION poly_term_cc

END MODULE poly_term_m
