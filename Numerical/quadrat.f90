module quadrat_m

  implicit none

contains

  subroutine quadrat(a, b, c, delta, root)

    ! This subroutine computes the real roots of a quadratic equation
    ! with real coefficients. If there is a double root, it appears in
    ! double in the output array. If there are two distinct roots,
    ! they are output in ascending order. If there is no real root,
    ! the output array is undefined.

    use assert_m, only: assert

    real, intent(in):: a, b, c
    ! a x^2 + b x + c = 0

    real, intent(out):: delta ! discriminant
    real, intent(out):: root(:) ! (2)

    ! Variables local to the procedure
    real q

    !---------------

    call assert(a /= 0., "quadrat a must be  /= 0") 
    delta = b**2 - 4. * a * c
    if (delta == 0.) then
       root = - b / 2 / a
    else if (delta > 0.) then
       q = - (b + sign(sqrt(delta), b)) / 2
       root(1) = q / a
       root(2) = c / q
       ! Sort the roots:
       if (root(1) > root(2)) root = root(2:1:-1)
    end if

  end subroutine quadrat

end module quadrat_m
