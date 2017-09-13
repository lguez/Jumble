MODULE outerprod_m

  IMPLICIT NONE

  INTERFACE outerprod
     MODULE PROCEDURE outerprod_r,outerprod_d
  END INTERFACE

  private outerprod_r,outerprod_d

CONTAINS

  FUNCTION outerprod_r(a,b)
    REAL, DIMENSION(:), INTENT(IN) :: a,b
    REAL, DIMENSION(size(a),size(b)) :: outerprod_r
    outerprod_r = spread(a,dim=2,ncopies=size(b)) * &
         spread(b,dim=1,ncopies=size(a))
  END FUNCTION outerprod_r

  FUNCTION outerprod_d(a,b)
    DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: a,b
    DOUBLE PRECISION, DIMENSION(size(a),size(b)) :: outerprod_d
    outerprod_d = spread(a,dim=2,ncopies=size(b)) * &
         spread(b,dim=1,ncopies=size(a))
  END FUNCTION outerprod_d

END MODULE outerprod_m
