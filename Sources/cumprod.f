module cumprod_m

  implicit none

contains

  RECURSIVE FUNCTION cumprod(arr,seed) RESULT(ans)

    ! Cumulative product on an array, with optional multiplicative seed.

    USE nrtype, only: wp

    REAL(WP), DIMENSION(:), INTENT(IN) :: arr
    REAL(WP), OPTIONAL, INTENT(IN) :: seed
    REAL(WP), DIMENSION(size(arr)) :: ans

    ! Local:
    INTEGER n,j
    REAL(WP) :: sd
    INTEGER, PARAMETER :: NPAR_CUMPROD=8

    !--------------------------------------------------------

    n=size(arr)
    if (n == 0) RETURN
    sd=1.0_wp
    if (present(seed)) sd=seed
    ans(1)=arr(1)*sd
    if (n < NPAR_CUMPROD) then
       do j=2,n
          ans(j)=ans(j-1)*arr(j)
       end do
    else
       ans(2:n:2)=cumprod(arr(2:n:2)*arr(1:n-1:2),sd)
       ans(3:n:2)=ans(2:n-1:2)*arr(3:n:2)
    end if

  END FUNCTION cumprod

end module cumprod_m
