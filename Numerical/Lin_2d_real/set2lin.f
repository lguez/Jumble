module set2lin_m

  implicit none

contains

  subroutine set2lin(a, b, found, x)

    ! This subroutine solves a set of 2 linear equations.

    use determin_m, only: determin

    real, intent(in):: a(:, :) ! (2, 2) matrix of coefficients
    real, intent(in):: b(:) ! (2) right-hand side
    logical, intent(out):: found ! a unique solution
    real, intent(out):: x(:) ! (2) unique solution

    ! Local variable:
    real det_a ! determinant of "a"

    !----------------------------------------------------------------

    det_a = determin(a)
    found = abs(det_a) > epsilon(0.)

    if (found) then
       x(1) = (b(1) * a(2, 2) - b(2) * a(1, 2)) / det_a
       x(2) = (a(1, 1) * b(2) - a(2, 1) * b(1)) / det_a
    end if

  end subroutine set2lin

end module set2lin_m
