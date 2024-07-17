module squeeze_m

  implicit none

contains

  pure function squeeze(string, c)

    ! Replaces adjacent duplicates of the specified character to a
    ! single occurrence of that character.

    character(len = :), allocatable:: squeeze
    character(len = *), intent(in):: string
    character, intent(in):: c

    ! Local:
    logical prev_target ! previous character was the target character
    logical curr_target ! current character is the target character
    integer i, i_squeezed
    character(len = len(string)) buffer

    !-------------------------------------------------------------------
    
    i_squeezed = 0
    prev_target = .false.
    
    do i = 1, len(string)
       curr_target = string(i:i) == c

       if (.not. (prev_target .and. curr_target)) then
          i_squeezed = i_squeezed + 1
          buffer(i_squeezed:i_squeezed) = string(i:i)
       end if
       
       prev_target = curr_target
    end do

    squeeze = buffer(:i_squeezed)

  end function squeeze
  
end module squeeze_m
