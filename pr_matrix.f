module pr_matrix_m

  implicit none

  private
  public pr_matrix
  interface pr_matrix
    ! Pretty printing of a real matrix. The difference between the
    ! specific procedures is the kind of argument a.
     module procedure s_pr_mat, d_pr_mat
  end interface

contains

  subroutine s_pr_mat(name, a)

    character(len=*), intent(in):: name
    real, intent(in):: a(:,:)

    character(len=20) fmt
    integer n_lines, n_col, i

    !-----------------

    n_lines = size(a, 1)
    n_col = size(a, 2)
    if (n_lines <= 10 .and. n_col <= 5) then
       print *, name, ":"
       write(unit=fmt, fmt='("(1p, ", i0, "(g10.3: 1X))")') n_col
       do i = 1, n_lines
          print fmt, a(i, :)
       end do
    else
       print *, '"', name, '" is too big to print.'
    end if

  end subroutine s_pr_mat

  !***********************************************************

  subroutine d_pr_mat(name, a)

    character(len=*), intent(in):: name
    double precision, intent(in):: a(:,:)

    character(len=20) fmt
    integer n_lines, n_col, i

    !-----------------

    n_lines = size(a, 1)
    n_col = size(a, 2)
    if (n_lines <= 10 .and. n_col <= 5) then
       print *, name, ":"
       write(unit=fmt, fmt='("(1p, ", i0, "(g8.1: 1X))")') n_col
       do i = 1, n_lines
          print fmt, a(i, :)
       end do
    else
       print *, '"', name, '" is too big to print.'
    end if

  end subroutine d_pr_mat

end module pr_matrix_m
