module differ_s_m

  implicit none

contains

  subroutine differ_s(differ, old, new, refer, diffmax, jexit)

    ! Compares the arrays old and new and gives to
    ! "differ" the value "true" if for one value of the index, the
    ! absolute difference between "old" and "new" is greater than or
    ! equal to "diffmax * refer". jexit is the lowest index value
    ! tested by the subroutine. jexit is an index for
    ! which a difference is found or jexit is equal to 1.

    real, intent(in):: old(:), new(:), refer(:) ! should have the same size
    real, intent(in):: diffmax
    logical, intent(out):: differ
    integer, intent(out), optional:: jexit

    ! Local variable:
    integer j

    !--------------------------------------------

    j = size(old)
    do
       differ = abs((new(j) - old(j)) / refer(j)) >= diffmax
       j = j - 1
       if (differ .or. (j < 1)) exit
    end do

    if (present(jexit)) jexit = j + 1

  end subroutine differ_s

end module differ_s_m
