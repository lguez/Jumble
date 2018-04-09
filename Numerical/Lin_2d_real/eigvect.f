module eigvect_m

  implicit none

contains

  pure function eigvect(a, lambda)

    ! This function computes the matrix of eigenvectors.

    real eigvect(2, 2)
    real, intent(in):: a(:, :), lambda(:)

    ! Variables local to the procedure:
    integer i0, j0, j, k, local(2)
    real b(2, 2)

    !----------------

    do k = 1, 2
       ! Compute column k of eigvect, corresponding to the k-th eigenvalue
       b = a
       forall (j = 1:2) b(j, j) = a(j, j) - lambda(k)
       ! (loss of precision if a(j, j) is close to lambda(k))
       ! Find a non-zero value in b:
       local = maxloc(abs(b))
       i0 = local(1); j0 = local(2)
       ! Solve line i0 of "b x = 0":
       j = merge(1, 2, j0 == 2) ! j /= j0
       eigvect(j, k) = 1
       eigvect(j0, k) = - b(i0, j) / b(i0, j0)
    end do

  end function eigvect

end module eigvect_m
