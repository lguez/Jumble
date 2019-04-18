MODULE diagadd_m

  IMPLICIT NONE

  INTERFACE diagadd
     ! Adds vector to diagonal of a matrix.
     MODULE PROCEDURE diagadd_rv,diagadd_r
  END INTERFACE

  private diagadd_rv,diagadd_r

CONTAINS

  SUBROUTINE diagadd_rv(mat,diag)
    use assert_eq_m, only: assert_eq
    REAL, DIMENSION(:,:), INTENT(INOUT) :: mat
    REAL, DIMENSION(:), INTENT(IN) :: diag
    INTEGER :: j,n
    n = assert_eq(size(diag),min(size(mat,1),size(mat,2)),'diagadd_rv')
    do j=1,n
       mat(j,j)=mat(j,j)+diag(j)
    end do
  END SUBROUTINE diagadd_rv

  SUBROUTINE diagadd_r(mat,diag)
    REAL, DIMENSION(:,:), INTENT(INOUT) :: mat
    REAL, INTENT(IN) :: diag
    INTEGER :: j,n
    n = min(size(mat,1),size(mat,2))
    do j=1,n
       mat(j,j)=mat(j,j)+diag
    end do
  END SUBROUTINE diagadd_r

END MODULE diagadd_m
