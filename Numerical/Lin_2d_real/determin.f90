module determin_m

  implicit none

contains

  pure real function determin(a)

    ! Computes the determinant of a (2, 2) real matrix.

    real, intent(in):: a(:, :) ! (2, 2)

    !-----------------------

    determin = A(1, 1) * A(2, 2) - A(2, 1) * A(1, 2)

  end function determin

end module determin_m
