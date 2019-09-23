module eigval_m

  implicit none

contains

  subroutine eigval(a, n, lambda)

    ! This subroutine computes real eigenvalues. Eigenvalues are
    ! returned in a size 2 array, in ascending order. If a double
    ! eigenvalue is found, the array contains this value in double.
    ! If no eigenvalue is found, the array is undefined.

    use determin_m, only: determin
    use quadrat_m, only: quadrat

    real, intent(in):: a(:, :) ! (2, 2)
    integer, intent(out):: n ! number of real eigenvalues found
    real, intent(out):: lambda(:) ! (2) eigenvalues

    ! Local:
    real trace, delta

    !---------------------

    trace = A(1, 1) + A(2, 2)

    ! Find the roots of the characteristic polynomial:
    call quadrat(1., - trace, determin(a), delta, lambda)
    if (delta > 0.) then
       n = 2
    else if (delta == 0.) then
       n = 1
    else
       n = 0
    end if

  end subroutine eigval

end module eigval_m
