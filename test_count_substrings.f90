program test_count_substrings

  use jumble, only: count_substrings, assert

  implicit none

  integer results(6)

  !-----------------------------------------------------------------------

  results = [count_substrings("the three truths", "th"), &
       count_Substrings("ababababab", "abab"), count_Substrings("", "abab"), &
       count_Substrings("ababababab", ""), count_Substrings("", ""), &
       count_substrings(" the three truths ", " ")]
  print *, results
  call assert(results == [3, 2, 0, 11, 1, 4], "test_count_substrings")

end program test_count_substrings
