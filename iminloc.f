module iminloc_m

  ! Useless in Fortran 95.

  implicit none

  interface iminloc
     module procedure iminloc_r, iminloc_d
  end interface

  private iminloc_r, iminloc_d

contains

  pure INTEGER FUNCTION iminloc_r(arr)
    REAL, INTENT(IN) :: arr(:)
    iminloc_r=minloc(arr, dim=1)
  END FUNCTION iminloc_r

  !******************************************************

  pure INTEGER FUNCTION iminloc_d(arr)
    DOUBLE PRECISION, INTENT(IN) :: arr(:)
    iminloc_d=minloc(arr, dim=1)
  END FUNCTION iminloc_d

end module iminloc_m
