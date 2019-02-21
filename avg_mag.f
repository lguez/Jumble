module avg_mag_m

  ! The generic procedure computes the average magnitude of an
  ! array. Magnitude of an element is log10 of absolute value. The
  ! difference between the specific procedures is the kind and rank of
  ! the array. We do not care here about precision so all specific
  ! procedures compute and return a default real kind value.

  implicit none

  interface avg_mag
     module procedure avg_mag1, avg_mag1_dble, avg_mag2, avg_mag2_dble, &
          avg_mag3, avg_mag3_dble, avg_mag4, avg_mag4_dble
  end interface

  private
  public avg_mag

contains

  pure real function avg_mag1(a)

    real, intent(in):: a(:)

    ! Local:
    logical not_zero(size(a)) ! not zero in "a"
    real magnit(size(a)) ! magnitudes of elements of "a"

    !-------------------------------------

    not_zero = a /= 0.

    if (any(not_zero)) then
       where (not_zero) magnit = log10(abs(a))
       avg_mag1 = sum(magnit, mask = not_zero) / count(not_zero)
    else
       avg_mag1 = - huge(0.) ! minus infinity
    end if

  end function avg_mag1

  !*******************************************************************

  pure real function avg_mag1_dble(a)

    double precision, intent(in):: a(:)

    !-------------------------------------

    avg_mag1_dble = avg_mag1(real(a))

  end function avg_mag1_dble

  !*******************************************************************

  pure real function avg_mag2(a)

    real, intent(in):: a(:, :)

    !-------------------------------------

    avg_mag2 = avg_mag1(pack(a, .true.))

  end function avg_mag2

  !*******************************************************************

  pure real function avg_mag2_dble(a)

    double precision, intent(in):: a(:, :)

    !-------------------------------------

    avg_mag2_dble = avg_mag1(pack(real(a), .true.))

  end function avg_mag2_dble

  !*******************************************************************

  pure real function avg_mag3(a)

    real, intent(in):: a(:, :, :)

    !-------------------------------------

    avg_mag3 = avg_mag1(pack(a, .true.))

  end function avg_mag3

  !*******************************************************************

  pure real function avg_mag3_dble(a)

    double precision, intent(in):: a(:, :, :)

    !-------------------------------------

    avg_mag3_dble = avg_mag1(pack(real(a), .true.))

  end function avg_mag3_dble

  !*******************************************************************

  pure real function avg_mag4(a)

    real, intent(in):: a(:, :, :, :)

    !-------------------------------------

    avg_mag4 = avg_mag1(pack(a, .true.))

  end function avg_mag4

  !*******************************************************************

  pure real function avg_mag4_dble(a)

    double precision, intent(in):: a(:, :, :, :)

    !-------------------------------------

    avg_mag4_dble = avg_mag1(pack(real(a), .true.))

  end function avg_mag4_dble

end module avg_mag_m
