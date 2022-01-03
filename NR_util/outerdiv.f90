module outerdiv_m

  implicit none

contains

  FUNCTION outerdiv(a,b)
    ! Returns a matrix that is the outer quotient of two vectors.
    USE nrtype, only: wp
    REAL(WP), DIMENSION(:), INTENT(IN) :: a,b
    REAL(WP), DIMENSION(size(a),size(b)) :: outerdiv
    outerdiv = spread(a,dim=2,ncopies=size(b)) / &
         spread(b,dim=1,ncopies=size(a))
  END FUNCTION outerdiv

end module outerdiv_m
