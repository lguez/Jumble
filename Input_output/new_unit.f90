module new_unit_m

  implicit none

contains

  subroutine new_unit(unit)

    ! This procedure provides a licit and not already opened external
    ! file unit. Note that if you call `new_unit` twice without
    ! opening a file between the two calls, then `new_unit` wil return
    ! the same value of `unit` for the two calls.

    integer, intent(out):: unit

    ! Local:
    logical opened, exist

    !------------------------------------------------------

    unit = 0

    do
       ! The collection of unit numbers that can be used in a program
       ! for external files is determined by the processor. The unit
       ! numbers that may be used are said to exist. There may be
       ! certain unit numbers that are never allowed for user files
       ! because they are restricted by the operating
       ! system. Cf. Adams (2009, § 9.1.6.1).
       inquire(unit = unit, opened = opened, exist = exist)
       if (exist .and. .not. opened) exit
       unit = unit + 1
    end do

  end subroutine new_unit

end module new_unit_m
