MODULE scatter_add_m

  ! Bug correction: intent of "dest".

  IMPLICIT NONE

  INTERFACE scatter_add
     MODULE PROCEDURE scatter_add_r,scatter_add_d
  END INTERFACE

  private scatter_add_r,scatter_add_d

CONTAINS

  SUBROUTINE scatter_add_r(dest,source,dest_index)
    use assert_eq_m, only: assert_eq
    REAL, DIMENSION(:), INTENT(inOUT) :: dest
    REAL, DIMENSION(:), INTENT(IN) :: source
    INTEGER, DIMENSION(:), INTENT(IN) :: dest_index
    INTEGER :: m,n,j,i
    n=assert_eq(size(source),size(dest_index),'scatter_add_r')
    m=size(dest)
    do j=1,n
       i=dest_index(j)
       if (i > 0 .and. i <= m) dest(i)=dest(i)+source(j)
    end do
  END SUBROUTINE scatter_add_r

  SUBROUTINE scatter_add_d(dest,source,dest_index)
    use assert_eq_m, only: assert_eq
    DOUBLE PRECISION, DIMENSION(:), INTENT(inOUT) :: dest
    DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: source
    INTEGER, DIMENSION(:), INTENT(IN) :: dest_index
    INTEGER :: m,n,j,i
    n=assert_eq(size(source),size(dest_index),'scatter_add_d')
    m=size(dest)
    do j=1,n
       i=dest_index(j)
       if (i > 0 .and. i <= m) dest(i)=dest(i)+source(j)
    end do
  END SUBROUTINE scatter_add_d

END MODULE scatter_add_m
