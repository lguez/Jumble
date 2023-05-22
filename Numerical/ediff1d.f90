module ediff1d_m

  IMPLICIT none

  interface ediff1d
     module procedure ediff1d_real, ediff1d_integer
  end interface ediff1d

  private
  public ediff1d

  ! Inspired by numpy.ediff1d. The argument a may have size 0 or 1,
  ! in which case the returned array has zero size.

contains

  function ediff1d_real(a)

    real, intent(in):: a(:)
    real ediff1d_real(size(a) - 1)

    ! local:
    integer i

    !---------------------------------------------------------------

    forall (i = 1:size(a) - 1) ediff1d_real(i) = a(i + 1) - a(i)

  end function ediff1d_real

  !********************************************************************

  function ediff1d_integer(a)

    integer, intent(in):: a(:)
    integer ediff1d_integer(size(a) - 1)

    ! local:
    integer i

    !---------------------------------------------------------------

    forall (i = 1:size(a) - 1) ediff1d_integer(i) = a(i + 1) - a(i)

  end function ediff1d_integer

end module ediff1d_m
