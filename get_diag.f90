MODULE get_diag_m

  IMPLICIT NONE

  INTERFACE get_diag
     ! Gets diagonal of a matrix.
     MODULE PROCEDURE get_diag_rv, get_diag_dv
  END INTERFACE

  private get_diag_rv, get_diag_dv

CONTAINS

  FUNCTION get_diag_rv(mat)
    use assert_eq_m, only: assert_eq
    REAL, DIMENSION(:,:), INTENT(IN) :: mat
    REAL, DIMENSION(size(mat,1)) :: get_diag_rv
    INTEGER j, n
    n=assert_eq(size(mat,1),size(mat,2),'get_diag_rv')
    do j=1, n
       get_diag_rv(j)=mat(j,j)
    end do
  END FUNCTION get_diag_rv

  !**************************************************************

  FUNCTION get_diag_dv(mat)
    use assert_eq_m, only: assert_eq
    DOUBLE PRECISION, DIMENSION(:,:), INTENT(IN) :: mat
    DOUBLE PRECISION, DIMENSION(size(mat,1)) :: get_diag_dv
    INTEGER j, n
    n=assert_eq(size(mat,1),size(mat,2),'get_diag_dv')
    do j=1, n
       get_diag_dv(j)=mat(j,j)
    end do
  END FUNCTION get_diag_dv

END MODULE get_diag_m
