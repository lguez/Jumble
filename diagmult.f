MODULE diagmult_m

  IMPLICIT NONE

  INTERFACE diagmult
     MODULE PROCEDURE diagmult_rv,diagmult_r
  END INTERFACE

  private diagmult_rv,diagmult_r

CONTAINS

  SUBROUTINE diagmult_rv(mat,diag)
    use assert_eq_m, only: assert_eq
    REAL, DIMENSION(:,:), INTENT(INOUT) :: mat
    REAL, DIMENSION(:), INTENT(IN) :: diag
    INTEGER j,n
    n = assert_eq(size(diag),min(size(mat,1),size(mat,2)),'diagmult_rv')
    do j=1,n
       mat(j,j)=mat(j,j)*diag(j)
    end do
  END SUBROUTINE diagmult_rv

  SUBROUTINE diagmult_r(mat,diag)
    REAL, DIMENSION(:,:), INTENT(INOUT) :: mat
    REAL, INTENT(IN) :: diag
    INTEGER j,n
    n = min(size(mat,1),size(mat,2))
    do j=1,n
       mat(j,j)=mat(j,j)*diag
    end do
  END SUBROUTINE diagmult_r

END MODULE diagmult_m
