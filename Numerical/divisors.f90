module divisors_m

  implicit none

contains

  pure function divisors(n)

    ! Returns all the divisors of a given integer, not sorted.

    integer, intent(in):: n ! must be >= 1
    integer, allocatable:: divisors(:)

    ! Variables local to the procedure:

    integer i, i_max
    integer n_div ! number of divisors of "n"
    integer work(n) ! temporary array to hold divisors

    !------------------------

    if (n == 1) then
       divisors = [1]
    else
       ! n >= 2, there are at least two divisors: 1 and "n"
       work(1) = 1
       work(2) = n
       n_div = 2

       i_max = int(sqrt(real(n)))

       do i = 2, i_max - 1
          if (mod(n, i) == 0) then
             work(n_div + 1) = i
             work(n_div + 2) = n / i
             n_div = n_div + 2
          end if
       end do

       if (i_max >= 2 .and. mod(n, i_max) == 0) then
          work(n_div + 1) = i_max
          n_div = n_div + 1
          
          if (i_max**2 /= n) then
             work(n_div + 1) = n / i_max
             n_div = n_div + 1
          end if
       end if

       divisors = work(:n_div)
    end if

  end function divisors

end module divisors_m
