module vabs_m

  implicit none

contains

  FUNCTION vabs(v)
    USE nrtype, only: wp
    REAL(WP), DIMENSION(:), INTENT(IN) :: v
    REAL(WP) :: vabs
    vabs=sqrt(dot_product(v,v))
  END FUNCTION vabs

end module vabs_m
