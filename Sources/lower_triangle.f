module lower_triangle_m

  implicit none

contains

  FUNCTION lower_triangle(j,k,extra)
    use arth_m, only: arth
    use outerdiff_m, only: outerdiff
    INTEGER, INTENT(IN) :: j,k
    INTEGER, OPTIONAL, INTENT(IN) :: extra
    LOGICAL, DIMENSION(j,k) :: lower_triangle
    INTEGER :: n
    n=0
    if (present(extra)) n=extra
    lower_triangle=(outerdiff(arth(1,1,j),arth(1,1,k)) > -n)
  END FUNCTION lower_triangle

end module lower_triangle_m
