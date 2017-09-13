module ifirstloc_m

  implicit none

contains

  pure INTEGER FUNCTION ifirstloc(mask, my_lbound)

    ! Location of first true value in a logical array, returned as an
    ! integer.

    ! If my_lbound is not present, returns size(mask) + 1 if mask has
    ! zero element or all elements of mask are false. So the result is
    ! always >= 1.

    ! If my_lbound is present, returns my_lbound + size(mask) if mask has
    ! zero element or all elements of mask are false. So the result is
    ! always >= my_lbound.

    LOGICAL, INTENT(IN):: mask(:)

    integer, optional, intent(in):: my_lbound
    ! lower bound of actual argument corresponding to mask

    ! Local:
    integer n

    !-------------------------------------------------------

    n = size(mask)
    ifirstloc = 1

    if (n >= 1) then
       do while (ifirstloc <= n - 1 .and. .not. mask(ifirstloc))
          ifirstloc = ifirstloc + 1
       end do
       ! {1 <= ifirstloc <= n}
       if (.not. mask(ifirstloc)) ifirstloc = n + 1
    end if

    if (present(my_lbound)) ifirstloc = ifirstloc + my_lbound - 1

  END FUNCTION ifirstloc

end module ifirstloc_m
