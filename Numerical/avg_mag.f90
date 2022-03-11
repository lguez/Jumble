module avg_mag_m

  implicit none

contains

  pure real function avg_mag(a)

    ! The procedure computes the average magnitude of an
    ! array. Magnitude of an element is log10 of absolute value.

    real, intent(in):: a(:) ! All elements should be defined.

    ! Local:
    logical not_zero(size(a)) ! not zero in "a"
    real magnit(size(a)) ! magnitudes of elements of "a"

    !-------------------------------------

    not_zero = a /= 0.

    if (any(not_zero)) then
       where (not_zero) magnit = log10(abs(a))
       avg_mag = sum(magnit, mask = not_zero) / count(not_zero)
    else
       avg_mag = - huge(0.) ! minus infinity
    end if

  end function avg_mag

end module avg_mag_m
