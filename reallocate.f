MODULE reallocate_m

  IMPLICIT NONE

  INTERFACE reallocate
     ! Reallocate pointer to new size, preserving its contents.
     MODULE PROCEDURE reallocate_rv, reallocate_rm, reallocate_iv, &
          reallocate_im, reallocate_hv
  END INTERFACE

  private
  public reallocate

CONTAINS

  FUNCTION reallocate_rv(p, n)
    REAL, POINTER:: p(:), reallocate_rv(:)
    INTEGER, INTENT(IN):: n
    INTEGER nold
    !-----------------------------------------------
    allocate(reallocate_rv(n))
    if (.not. associated(p)) RETURN
    nold = size(p)
    reallocate_rv(:min(nold, n)) = p(:min(nold, n))
    deallocate(p)
  END FUNCTION reallocate_rv

  !***********************************************************

  FUNCTION reallocate_iv(p, n)
    INTEGER, POINTER:: p(:), reallocate_iv(:)
    INTEGER, INTENT(IN):: n
    INTEGER nold
    !-----------------------------------------------
    allocate(reallocate_iv(n))
    if (.not. associated(p)) RETURN
    nold = size(p)
    reallocate_iv(:min(nold, n)) = p(:min(nold, n))
    deallocate(p)
  END FUNCTION reallocate_iv

  !***********************************************************

  FUNCTION reallocate_hv(p, n)
    CHARACTER, POINTER:: p(:), reallocate_hv(:)
    INTEGER, INTENT(IN):: n
    INTEGER nold
    !-----------------------------------------------
    allocate(reallocate_hv(n))
    if (.not. associated(p)) RETURN
    nold = size(p)
    reallocate_hv(:min(nold, n)) = p(:min(nold, n))
    deallocate(p)
  END FUNCTION reallocate_hv

  !***********************************************************

  FUNCTION reallocate_rm(p, n, m)
    REAL, DIMENSION(:, :), POINTER:: p, reallocate_rm
    INTEGER, INTENT(IN):: n, m
    INTEGER nold, mold
    !-----------------------------------------------
    allocate(reallocate_rm(n, m))
    if (.not. associated(p)) RETURN
    nold = size(p, 1)
    mold = size(p, 2)
    reallocate_rm(:min(nold, n), :min(mold, m)) &
         = p(:min(nold, n), :min(mold, m))
    deallocate(p)
  END FUNCTION reallocate_rm

  !***********************************************************

  FUNCTION reallocate_im(p, n, m)
    INTEGER, DIMENSION(:, :), POINTER:: p, reallocate_im
    INTEGER, INTENT(IN):: n, m
    INTEGER nold, mold
    !-----------------------------------------------
    allocate(reallocate_im(n, m))
    if (.not. associated(p)) RETURN
    nold = size(p, 1)
    mold = size(p, 2)
    reallocate_im(:min(nold, n), :min(mold, m)) &
         = p(:min(nold, n), :min(mold, m))
    deallocate(p)
  END FUNCTION reallocate_im

END MODULE reallocate_m
