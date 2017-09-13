module zroots_unity_m

  implicit none

contains

  FUNCTION zroots_unity(n, nn)

    ! Returns nn powers of the nth root of unity.

    USE nrtype, only: wp, twopi

    INTEGER, INTENT(IN) :: n, nn
    COMPLEX(WP) zroots_unity(nn)

    ! Local:
    INTEGER k
    REAL(WP) :: theta

    !-------------------------------------------------

    zroots_unity(1) = 1.
    theta = TWOPI / n
    k=1
    do
       if (k >= nn) exit
       zroots_unity(k + 1) = cmplx(cos(k * theta), sin(k * theta), WP)
       zroots_unity(k + 2: min(2 * k, nn)) = zroots_unity(k + 1) &
            * zroots_unity(2: min(k, nn - k))
       k = 2 * k
    end do

  END FUNCTION zroots_unity

end module zroots_unity_m
