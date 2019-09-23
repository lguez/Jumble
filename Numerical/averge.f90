module AVERGE_m

  implicit none

contains

  pure real FUNCTION AVERGE(X,Y)

    ! Arithmetic average of arithmetic and geometric averages.

    real, intent(in):: x, y

    !-------------

    AVERGE = .5 * SQRT(X) * sqrt(Y) + 0.25 * (X + Y)
    ! (the square roots should be separated to avoid underflow)

  END FUNCTION AVERGE

end module AVERGE_m
