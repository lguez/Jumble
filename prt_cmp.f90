module prt_cmp_m

  implicit none

  interface prt_cmp
     ! Used by "compare". Only useful for "compare" so do not use this
     ! module in the grouping module jumble.
     module procedure prt_cmp_real, prt_cmp_double
  end interface prt_cmp

  private
  public prt_cmp

contains

  subroutine prt_cmp_real(data_old, data_new, maxv, location)

    real, intent(in):: data_old, data_new, maxv
    integer, intent(in):: location(:)

    ! Variable local to the procedure:
    character(len=30) format_string

    !------------------------------------------------------

    if (size(location) == 1) then
       format_string = "(a, i0, a)"
    else
       ! size(location) >= 2
       write(unit=format_string, fmt="(a, i0, a)") "(a, ", &
            size(location) - 1, "(i0, 1x), i0, a)"
    end if

    print *
    print '("Maximum difference: ", 1pg8.1)', maxv
!!    print '("Maximum difference: ", 1pg9.2)', maxv
    print *, 'Occurring at:'

    write(unit=*, fmt=format_string, advance="no") 'data_old(', location, &
         ') = '
    print *, data_old

    write(unit=*, fmt=format_string, advance="no") 'data_new(', location, &
         ') = '
    print *, data_new

  end subroutine prt_cmp_real

  !***********************************************************

  subroutine prt_cmp_double(data_old, data_new, maxv, location)

    double precision, intent(in):: data_old, data_new, maxv
    integer, intent(in):: location(:)

    ! Variable local to the procedure:
    character(len=30) format_string

    !------------------------------------------------------

    if (size(location) == 1) then
       format_string = "(a, i0, a)"
    else
       ! size(location) >= 2
       write(unit=format_string, fmt="(a, i0, a)") "(a, ", &
            size(location) - 1, "(i0, 1x), i0, a)"
    end if

    print *
    print '("Maximum difference: ", 1pg8.1)', maxv
!!    print '("Maximum difference: ", 1pg9.2)', maxv
    print *, 'Occurring at:'

    write(unit=*, fmt=format_string, advance="no") 'data_old(', location, &
         ') = '
    print *, data_old

    write(unit=*, fmt=format_string, advance="no") 'data_new(', location, &
         ') = '
    print *, data_new

  end subroutine prt_cmp_double

end module prt_cmp_m
