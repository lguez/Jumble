MODULE imaxloc_m

  ! Useless in Fortran 95.

  implicit none

  INTERFACE imaxloc
     MODULE PROCEDURE imaxloc_r,imaxloc_i
  END INTERFACE

  private imaxloc_r,imaxloc_i

CONTAINS

  INTEGER FUNCTION imaxloc_r(arr)
    REAL, INTENT(IN) :: arr(:)
    imaxloc_r=maxloc(arr, dim=1)
  END FUNCTION imaxloc_r

  INTEGER FUNCTION imaxloc_i(iarr)
    INTEGER, INTENT(IN) :: iarr(:)
    imaxloc_i=maxloc(iarr, dim=1)
  END FUNCTION imaxloc_i

END MODULE imaxloc_m
