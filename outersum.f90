module outersum_m

  implicit none

contains

  FUNCTION outersum(a,b)

    ! Returns the outer sum of two vectors.
    
    USE nrtype, only: wp
    REAL(WP), DIMENSION(:), INTENT(IN) :: a,b
    REAL(WP), DIMENSION(size(a),size(b)) :: outersum
    outersum = spread(a,dim=2,ncopies=size(b)) + &
         spread(b,dim=1,ncopies=size(a))
  END FUNCTION outersum

end module outersum_m
