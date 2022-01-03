module upper_triangle_m

  implicit none

contains

  FUNCTION upper_triangle(j, k, extra)

    ! Returns an upper triangular mask.

    ! When the optional argument extra is zero or absent, returns a
    ! logical mask of shape (j, k) whose values are true above and to
    ! the right of the diagonal, false elsewhere (including on the
    ! diagonal). When extra is present and positive, a corresponding
    ! number of additional (sub-)diagonals are returned as
    ! true. (extra = 1 makes the main diagonal return true.) When
    ! extra is present and negative, it suppresses a corresponding
    ! number of superdiagonals.

    use arth_m, only: arth
    use outerdiff_m, only: outerdiff

    INTEGER, INTENT(IN):: j, k
    INTEGER, OPTIONAL, INTENT(IN):: extra
    LOGICAL upper_triangle(j, k)

    ! Local:
    INTEGER n

    !--------------------------------------------------

    n = 0
    if (present(extra)) n = extra
    upper_triangle = outerdiff(arth(1, 1, j), arth(1, 1, k)) < n

  END FUNCTION upper_triangle

end module upper_triangle_m
