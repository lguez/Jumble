module spherical

  implicit none

contains


  subroutine rectsph(rect,col,azm,r,r2)

    ! Converts rectangular coordinates to spherical coordinates.
    ! (Ox) is taken as the polar axis.
    ! Azimuth is the angle in the (Oyz) plane, measured from vector y.
    ! If neither "r" nor "r2" is present then they are assumed to be
    ! equal to 1.

    real, intent(in):: rect(3)
    ! (Rectangular coordinates x,y,z. Should be different from (0,0
    ! ,0))

    real, intent(out):: col ! Colatitude (in radians)
    real, intent(out):: azm ! Azimuth (in radians). -pi < azm <= pi
    real, intent(out), optional:: r ! Radius
    real, intent(out), optional:: r2 ! Square of radius

    ! Local variables:
    logical p, p2
    real d ! Radius
    real d2 ! Square of radius

    !----------------------------------------------------------------

    p = present(r)
    p2 = present(r2)

    if (.not. p .and. .not. p2) then
       ! Distance to the origin is assumed to be 1
       col = acos(rect(1))
    else
       ! Compute the distance to the origin:
       d2 = dot_product(rect,rect)
       d = sqrt(d2)
       col = acos(rect(1)/d)
       if (p) r = d
       if (p2) r2 = d2
    end if

    if (rect(2) == 0. .and. rect(3) == 0.) then
       azm = 0.
       ! (arbitrary value, azimuth is not well defined since vector
       !  position is parallel
       !  to polar axis)
    else
       azm = atan2(rect(3),rect(2))
    end if

  end subroutine rectsph

  !*******************************************************************

  real function sphbase(col,azm)

    ! This function returns the matrix of the spherical base: (radial
    ! vector, colatitude vector, azimuthal vector) in the cartesian
    ! vector base: (x, y, z).  Vector x is assumed to be the polar
    ! vector. Colatitude "col" and azimuth "azm" are the angular
    ! spherical coordinates of the radial vector (azimuth is measured
    ! from vector "y"). Note that if col = 0 (radial vector parallel
    ! to x) then the choice of either the colatitude vector or the
    ! azimuthal vector is arbitrary. The choice made depends on the
    ! input value of azm. If azm is 0 then sphbase will return vector
    ! y as the colatitude vector ("sphbase" is then the identity
    ! matrix). Uses "sphrect", which converts spherical coordinates to
    ! rectangular coordinates.

    dimension sphbase(3,3) ! Transformation matrix (no dimension)

    real, intent(in):: col ! Colatitude (in radians)
    real, intent(in):: azm ! Azimuth (in radians)

    ! Local variable:
    real pi

    !-----------------------------------------------------------------

    pi = acos(-1.)

    ! Radial vector:
    sphbase(:,1) = sphrect(col, azm)

    ! Colatitude vector:
    sphbase(:,2) = sphrect(col + pi/2, azm)

    ! Azimuthal vector:
    sphbase(:,3) = (/0., - sin(azm), cos(azm)/)

  end function sphbase

  !*****************************************************************

  real function sphrect(col,azm,r)

    ! Converts spherical coordinates to rectangular coordinates. (Ox)
    ! is taken as the polar axis. Azimuth "azm" is the angle in the
    ! (Oyz) plane, measured from vector y.
    ! If r is not present then we assume r = 1.

    dimension sphrect(3) ! Rectangular coordinates = (x,y,z)

    real, intent(in):: col ! Colatitude (in radians)
    real, intent(in):: azm ! Azimuth (in radians)
    real, intent(in), optional:: r ! Radius

    !---------------------------------------------------------------

    sphrect = (/cos(col), sin(col) * cos(azm), sin(col) * sin(azm)/)
    if (present(r)) sphrect = sphrect * r

  end function sphrect

end module spherical
