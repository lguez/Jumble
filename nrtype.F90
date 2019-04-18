MODULE nrtype

  implicit none

  integer, parameter:: wp = CPP_WP ! working precision for real type

  ! Frequently used mathematical constants (with precision to spare):

  REAL, PARAMETER :: PI=acos(-1.)
  REAL, PARAMETER :: PIO2=pi / 2.
  REAL, PARAMETER :: TWOPI=2. * pi
  REAL, PARAMETER :: SQRT2=sqrt(2.)
  REAL, PARAMETER :: EULER=0.5772156649015328606065120900824024310422

  DOUBLE PRECISION, PARAMETER:: PI_D = acos(- 1d0)
  DOUBLE PRECISION, PARAMETER:: PIO2_D = PI_D / 2d0
  DOUBLE PRECISION, PARAMETER:: TWOPI_D= 2d0 * pi_d

  real, parameter:: deg_to_rad = pi / 180. ! degrees to radians
  real, parameter:: rad_to_deg = 180. / pi ! radians to degrees
  
  ! Derived data types for sparse matrices, single and double
  ! precision (see use in Chapter B2):

  TYPE sprs2_sp
     INTEGER :: n,len
     REAL, DIMENSION(:), POINTER :: val
     INTEGER, DIMENSION(:), POINTER :: irow
     INTEGER, DIMENSION(:), POINTER :: jcol
  END TYPE sprs2_sp

  TYPE sprs2_dp
     INTEGER :: n,len
     DOUBLE PRECISION, DIMENSION(:), POINTER :: val
     INTEGER, DIMENSION(:), POINTER :: irow
     INTEGER, DIMENSION(:), POINTER :: jcol
  END TYPE sprs2_dp

END MODULE nrtype
