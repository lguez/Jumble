module count_substrings_m

  implicit none

contains

  pure integer function count_substrings(string, substring)

    ! This function returns the number of non-overlapping occurrences
    ! of substring in string. If substring is empty, the function
    ! returns the length of string plus one, which is the number of
    ! empty strings between characters (as in string method `count` in
    ! Python). If string is empty and substring is not empty then the
    ! function returns 0.

    ! Worst case: when len(substring) = len(string) / 2 and there is 0
    ! match, the number of comparisons of single characters is of the
    ! order of len(string)^2.

    ! Adapted from:

    ! https://rosettacode.org/wiki/Count_occurrences_of_a_substring#Fortran

    character(len = *), intent(in):: string, substring

    ! Local:
    integer delta, i1, i

    !--------------------------------------------------------------------

    delta = len(substring) - 1

    if (delta == - 1) then
       count_substrings = len(string) + 1
    else
       i1 = 1
       count_substrings = 0

       do
          i = index(string(i1:), substring)
          if (i == 0) exit
          count_substrings = count_substrings + 1
          i1 = i1 + i + delta
       end do
    end if

  end function count_substrings

end module count_substrings_m
