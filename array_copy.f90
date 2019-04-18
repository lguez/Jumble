MODULE array_copy_m

  IMPLICIT NONE

  INTERFACE array_copy
     ! Copies to a destination array dest the one-dimensional array
     ! src, or as much of src as will fit in dest. Returns the number
     ! of components copied as n_copied, and the number of components
     ! not copied as n_not_copied. The main use of this utility is
     ! where src is an expression that returns an array whose size is
     ! not known in advance, for example, the value returned by the
     ! pack intrinsic.
     MODULE PROCEDURE array_copy_r, array_copy_d, array_copy_i
  END INTERFACE array_copy

  private array_copy_r, array_copy_d, array_copy_i

CONTAINS

  SUBROUTINE array_copy_r(src, dest, n_copied, n_not_copied)
    REAL, INTENT(IN) :: src(:)
    REAL, INTENT(OUT) :: dest(:)
    INTEGER, INTENT(OUT) :: n_copied, n_not_copied
    n_copied=min(size(src), size(dest))
    n_not_copied=size(src)-n_copied
    dest(:n_copied)=src(:n_copied)
  END SUBROUTINE array_copy_r

  !***************************************************************
  
  SUBROUTINE array_copy_d(src, dest, n_copied, n_not_copied)
    double precision, INTENT(IN) :: src(:)
    double precision, INTENT(OUT) :: dest(:)
    INTEGER, INTENT(OUT) :: n_copied, n_not_copied
    n_copied=min(size(src), size(dest))
    n_not_copied=size(src)-n_copied
    dest(:n_copied)=src(:n_copied)
  END SUBROUTINE array_copy_d

  !***************************************************************
  
  SUBROUTINE array_copy_i(src, dest, n_copied, n_not_copied)
    INTEGER, INTENT(IN) :: src(:)
    INTEGER, INTENT(OUT) :: dest(:)
    INTEGER, INTENT(OUT) :: n_copied, n_not_copied
    n_copied=min(size(src), size(dest))
    n_not_copied=size(src)-n_copied
    dest(:n_copied)=src(:n_copied)
  END SUBROUTINE array_copy_i

END MODULE array_copy_m
