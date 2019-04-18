MODULE put_diag_m

  IMPLICIT NONE

  INTERFACE put_diag
     ! Sets diagonal of a matrix.
     MODULE PROCEDURE put_diag_rv, put_diag_r
  END INTERFACE

  private put_diag_rv, put_diag_r

CONTAINS

  SUBROUTINE put_diag_rv(diagv,mat)
    use assert_eq_m, only: assert_eq
    REAL, DIMENSION(:), INTENT(IN) :: diagv
    REAL, DIMENSION(:,:), INTENT(INOUT) :: mat
    INTEGER :: j,n
    n=assert_eq(size(diagv),min(size(mat,1),size(mat,2)),'put_diag_rv')
    do j=1,n
       mat(j,j)=diagv(j)
    end do
  END SUBROUTINE put_diag_rv

  SUBROUTINE put_diag_r(scal,mat)
    REAL, INTENT(IN) :: scal
    REAL, DIMENSION(:,:), INTENT(INOUT) :: mat
    INTEGER :: j,n
    n = min(size(mat,1),size(mat,2))
    do j=1,n
       mat(j,j)=scal
    end do
  END SUBROUTINE put_diag_r

END MODULE put_diag_m
