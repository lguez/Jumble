module ediff1d_m

  IMPLICIT none

contains

  function ediff1d(a)

    ! Inspired by numpy.ediff1d

    real, intent(in):: a(:)
    real ediff1d(size(a) - 1)

    ! local:
    integer i

    !---------------------------------------------------------------

    forall (i = 1:size(a) - 1) ediff1d(i) = a(i + 1) - a(i)

  end function ediff1d

end module ediff1d_m
