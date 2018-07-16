MODULE swap_m

  IMPLICIT NONE

  INTERFACE swap
     ! Swap elements of two input arguments corresponding to input mask.
     MODULE PROCEDURE swap_i, swap_r, swap_rv, swap_rm, swap_c, swap_cv, &
          swap_cm, swap_z, swap_zv, swap_zm, swap_d, swap_dv
  END INTERFACE

  private
  public swap

CONTAINS

  SUBROUTINE swap_i(a, b, mask)
    INTEGER, INTENT(INOUT) :: a, b
    LOGICAL, INTENT(IN), optional :: mask
    INTEGER :: dum
    !-------------------------------
    include "swap_scalar.h"
  END SUBROUTINE swap_i

  !************************************************

  SUBROUTINE swap_r(a, b, mask)
    REAL, INTENT(INOUT) :: a, b
    LOGICAL, INTENT(IN), optional :: mask
    REAL :: dum
    !-------------------------------
    include "swap_scalar.h"
  END SUBROUTINE swap_r

  !************************************************

  SUBROUTINE swap_rv(a, b, mask)
    REAL, DIMENSION(:), INTENT(INOUT) :: a, b
    LOGICAL, DIMENSION(:), INTENT(IN), optional :: mask
    REAL, DIMENSION(size(a)) :: dum
    !-------------------------------
    include "swap_array.h"
  END SUBROUTINE swap_rv

  !************************************************

  SUBROUTINE swap_rm(a, b, mask)
    REAL, DIMENSION(:, :), INTENT(INOUT) :: a, b
    LOGICAL, DIMENSION(:, :), INTENT(IN), optional :: mask
    REAL, DIMENSION(size(a, 1), size(a, 2)) :: dum
    !-------------------------------
    include "swap_array.h"
  END SUBROUTINE swap_rm

  !************************************************

  SUBROUTINE swap_c(a, b, mask)
    COMPLEX, INTENT(INOUT) :: a, b
    LOGICAL, INTENT(IN), optional :: mask
    COMPLEX :: dum
    !-------------------------------
    include "swap_scalar.h"
  END SUBROUTINE swap_c

  !************************************************

  SUBROUTINE swap_cv(a, b, mask)
    COMPLEX, DIMENSION(:), INTENT(INOUT) :: a, b
    LOGICAL, DIMENSION(:), INTENT(IN), optional :: mask
    COMPLEX, DIMENSION(SIZE(a)) :: dum
    !-------------------------------
    include "swap_array.h"
  END SUBROUTINE swap_cv

  !************************************************

  SUBROUTINE swap_cm(a, b, mask)
    COMPLEX, DIMENSION(:, :), INTENT(INOUT) :: a, b
    LOGICAL, DIMENSION(:, :), INTENT(IN), optional :: mask
    COMPLEX, DIMENSION(size(a, 1), size(a, 2)) :: dum
    !-------------------------------
    include "swap_array.h"
  END SUBROUTINE swap_cm

  !************************************************

  SUBROUTINE swap_z(a, b, mask)
    COMPLEX(KIND(0D0)), INTENT(INOUT) :: a, b
    LOGICAL, INTENT(IN), optional :: mask
    COMPLEX(KIND(0D0)) :: dum
    !-------------------------------
    include "swap_scalar.h"
  END SUBROUTINE swap_z

  !************************************************

  SUBROUTINE swap_zv(a, b, mask)
    COMPLEX(KIND(0D0)), DIMENSION(:), INTENT(INOUT) :: a, b
    LOGICAL, DIMENSION(:), INTENT(IN), optional :: mask
    COMPLEX(KIND(0D0)), DIMENSION(SIZE(a)) :: dum
    !-------------------------------
    include "swap_array.h"
  END SUBROUTINE swap_zv

  !************************************************

  SUBROUTINE swap_zm(a, b, mask)
    COMPLEX(KIND(0D0)), DIMENSION(:, :), INTENT(INOUT) :: a, b
    LOGICAL, DIMENSION(:, :), INTENT(IN), optional :: mask
    COMPLEX(KIND(0D0)), DIMENSION(size(a, 1), size(a, 2)) :: dum
    !-------------------------------
    include "swap_array.h"
  END SUBROUTINE swap_zm

  !************************************************

  SUBROUTINE swap_d(a, b, mask)
    DOUBLE PRECISION, INTENT(INOUT) :: a, b
    LOGICAL, INTENT(IN), optional :: mask
    DOUBLE PRECISION :: dum
    !-------------------------------
    include "swap_scalar.h"
  END SUBROUTINE swap_d

  !************************************************

  SUBROUTINE swap_dv(a, b, mask)
    DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: a, b
    LOGICAL, DIMENSION(:), INTENT(IN), optional :: mask
    DOUBLE PRECISION, DIMENSION(size(a)) :: dum
    !-------------------------------
    include "swap_array.h"
  END SUBROUTINE swap_dv

END MODULE swap_m
