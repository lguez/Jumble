module inv_mat_m

  implicit none

contains

  pure function inv_mat(r)

    ! This function computes the inverse of a 2 * 2 matrix.

    use determin_m, only: determin

    real inv_mat(2, 2)
    real, intent(in):: r(:, :)

    ! Variables local to the procedure:
    integer i, j

    !------------------------

    forall (i = 1:2, j = 1:2)
       inv_mat(i, j) = (-1)**(i + j) * r(3 - i, 3 - j) ! cofactor
    end forall
    inv_mat = transpose(inv_mat) / determin(R)

  end function inv_mat

end module inv_mat_m

