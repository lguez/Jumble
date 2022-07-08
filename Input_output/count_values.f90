module count_values_m

  implicit none

contains

  subroutine count_values(unit, n)

    ! Count the number of values per line. Does not work with several
    ! adjacent commas.
   
    integer, intent(in):: unit ! logical unit for input file
    integer, intent(out):: n

    ! Local:
    logical curr_value ! current character is part of a value
    integer iostat
    character c
    logical prev_value ! previous character was part of a value

    !-------------------------------------------------------------------
    
    n = 0
    curr_value = .false.
    
    do
       read(unit, fmt = '(a)', advance = 'no', iostat = iostat) c
       if (iostat /= 0) exit
       prev_value = curr_value
       curr_value = c /= " " .and. c /= ","
       if (curr_value .and. .not. prev_value) n = n + 1
    end do

  end subroutine count_values
  
end module count_values_m
