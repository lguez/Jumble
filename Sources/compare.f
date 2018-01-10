module compare_m

  use prt_cmp_m, only: prt_cmp
  use avg_mag_m, only: avg_mag

  implicit none

  interface compare
     ! Makes a numerical comparison between two arrays of rank 1 to 4,
     ! of types real or double precision.
     module procedure compare1, compare1_dble, compare2, compare2_dble, &
          compare3, compare3_dble, compare4, compare4_dble
  end interface

  character(len=*), parameter:: dashes &
       = "---------------------------------------------"

  private
  public compare

contains

  subroutine compare1(data_old, data_new, tag, comp_mag, report_id, quiet)

    ! Rank 1, real

    use nr_util, only: assert_eq

    real, intent(in):: data_old(:), data_new(:)
    character(len=*), intent(in):: tag
    logical, intent(in):: comp_mag
    logical, intent(in):: report_id ! report identical variables
    logical, intent(in):: quiet

    ! Variables local to the subprogram:

    logical zero(size(data_old))

    real rel_diff(size(data_old))
    ! (absolute value of relative difference)

    real abs_diff(size(data_old))
    ! (absolute value of absolute difference)

    integer data_size
    integer location(1)
    character(len=len(dashes)+len(tag)+20) tag_fmt

    !------------------------------------------------------

    data_size = assert_eq(size(data_old), size(data_new), &
         "compare1: sizes differ -- " // tag)
    if (quiet) then
       tag_fmt = '(a, ":")'
    else
       tag_fmt = '(/, "' // dashes // '", /, a, ":")'
    end if

    if (data_size == 0) then
       print tag_fmt, tag
       print *, "Zero-sized array"
    else
       if (all(data_old == data_new)) then
          if (report_id) then
             write(unit=*, fmt=tag_fmt, advance="no") tag
             print *, "arrays are identical."
          end if
       else
          if (quiet) then
             write(unit=*, fmt=tag_fmt, advance="no") tag
             print *, "arrays are different."
          else
             zero = data_old == 0. .or. data_new == 0.
             where (.not. zero) rel_diff = abs(data_new / data_old - 1.)
             abs_diff = abs(data_new - data_old)

             print tag_fmt, tag

             if (comp_mag) then
                print *
                print '("Average value of log10(abs(data_old)): ", 1pg8.1)', &
                     avg_mag(data_old)
             end if

             if (any(.not. zero)) then
                print *
                print *, 'Relative difference for non-zero values:'
                location = maxloc(rel_diff, mask=.not. zero)
                call prt_cmp(data_old(location(1)), data_new(location(1)), &
                     rel_diff(location(1)), location)
             end if

             if (any(zero)) then
                print *
                print *, 'Absolute difference when there is a zero:'
                location = maxloc(abs_diff, mask=zero)
                call prt_cmp(data_old(location(1)), data_new(location(1)), &
                     abs_diff(location(1)), location)
             end if

             print *
             print *, 'Absolute difference:'
             location = maxloc(abs_diff)
             call prt_cmp(data_old(location(1)), data_new(location(1)), &
                  abs_diff(location(1)), location)
          end if
       end if
    end if

  end subroutine compare1

  !***********************************************************

  subroutine compare1_dble(data_old, data_new, tag, comp_mag, report_id, quiet)

    ! Rank 1, double precision

    use nr_util, only: assert_eq

    double precision, intent(in):: data_old(:), data_new(:)
    character(len=*), intent(in):: tag
    logical, intent(in):: comp_mag
    logical, intent(in):: report_id ! report identical variables
    logical, intent(in):: quiet

    ! Variables local to the subprogram:

    logical zero(size(data_old))

    double precision rel_diff(size(data_old))
    ! (absolute value of relative difference)

    double precision abs_diff(size(data_old))
    ! (absolute value of absolute difference)

    integer data_size
    integer location(1)
    character(len=len(dashes)+len(tag)+20) tag_fmt

    !------------------------------------------------------

    data_size = assert_eq(size(data_old), size(data_new), &
         "compare1_dble: sizes differ -- " // tag)
    if (quiet) then
       tag_fmt = '(a, ":")'
    else
       tag_fmt = '(/, "' // dashes // '", /, a, ":")'
    end if

    if (data_size == 0) then
       print tag_fmt, tag
       print *, "Zero-sized array"
    else
       if (all(data_old == data_new)) then
          if (report_id) then
             write(unit=*, fmt=tag_fmt, advance="no") tag
             print *, "arrays are identical."
          end if
       else
          if (quiet) then
             write(unit=*, fmt=tag_fmt, advance="no") tag
             print *, "arrays are different."
          else
             zero = data_old == 0d0 .or. data_new == 0d0
             where (.not. zero) rel_diff = abs(data_new / data_old - 1d0)
             abs_diff = abs(data_new - data_old)

             print tag_fmt, tag

             if (comp_mag) then
                print *
                print '("Average value of log10(abs(data_old)): ", 1pg8.1)', &
                     avg_mag(data_old)
             end if

             if (any(.not. zero)) then
                print *
                print *, 'Relative difference for non-zero values:'
                location = maxloc(rel_diff, mask=.not. zero)
                call prt_cmp(data_old(location(1)), data_new(location(1)), &
                     rel_diff(location(1)), location)
             end if

             if (any(zero)) then
                print *
                print *, 'Absolute difference when there is a zero:'
                location = maxloc(abs_diff, mask=zero)
                call prt_cmp(data_old(location(1)), data_new(location(1)), &
                     abs_diff(location(1)), location)
             end if

             print *
             print *, 'Absolute difference:'
             location = maxloc(abs_diff)
             call prt_cmp(data_old(location(1)), data_new(location(1)), &
                  abs_diff(location(1)), location)
          end if
       end if
    end if

  end subroutine compare1_dble

  !***********************************************************

  subroutine compare2(data_old, data_new, tag, comp_mag, report_id, quiet)

    ! Rank 2, real

    use nr_util, only: assert
    use point_m, only: point

    real, intent(in):: data_old(:,:), data_new(:,:)
    character(len=*), intent(in):: tag
    logical, intent(in):: comp_mag
    logical, intent(in):: report_id ! report identical variables
    logical, intent(in):: quiet

    ! Variables local to the subprogram:

    logical zero(size(data_old,1), size(data_old,2))

    real rel_diff(size(data_old,1), size(data_old,2))
    ! (absolute value of relative difference)

    real abs_diff(size(data_old,1), size(data_old,2))
    ! (absolute value of absolute difference)

    integer location(2)
    character(len=len(dashes)+len(tag)+20) tag_fmt

    !------------------------------------------------------

    call assert(shape(data_old) == shape(data_new), &
         "compare2: shapes differ -- " // tag)
    if (quiet) then
       tag_fmt = '(a, ":")'
    else
       tag_fmt = '(/, "' // dashes // '", /, a, ":")'
    end if

    if (size(data_old) == 0) then
       print tag_fmt, tag
       print *, "Zero-sized array"
    else
       if (all(data_old == data_new)) then
          if (report_id) then
             write(unit=*, fmt=tag_fmt, advance="no") tag
             print *, "arrays are identical."
          end if
       else
          if (quiet) then
             write(unit=*, fmt=tag_fmt, advance="no") tag
             print *, "arrays are different."
          else
             zero = data_old == 0. .or. data_new == 0.
             where (.not. zero) rel_diff = abs(data_new / data_old - 1.)
             abs_diff = abs(data_new - data_old)

             print tag_fmt, tag

             if (comp_mag) then
                print *
                print '("Average value of log10(abs(data_old)): ", 1pg8.1)', &
                     avg_mag(data_old)
             end if

             if (any(.not. zero)) then
                print *
                print *, 'Relative difference for non-zero values:'
                location = maxloc(rel_diff, mask=.not. zero)
                call prt_cmp(point(data_old, location), &
                     point(data_new, location), point(rel_diff, location), &
                     location)
             end if

             if (any(zero)) then
                print *
                print *, 'Absolute difference when there is a zero:'
                location = maxloc(abs_diff, mask=zero)
                call prt_cmp(point(data_old, location), &
                     point(data_new, location), point(abs_diff, location), &
                     location)
             end if

             print *
             print *, 'Absolute difference:'
             location = maxloc(abs_diff)
             call prt_cmp(point(data_old, location), &
                  point(data_new, location), point(abs_diff, location), &
                  location)
          end if
       end if
    end if

  end subroutine compare2

  !***********************************************************

  subroutine compare2_dble(data_old, data_new, tag, comp_mag, report_id, quiet)

    ! Rank 2, double precision

    use nr_util, only: assert
    use point_m, only: point

    double precision, intent(in):: data_old(:,:), data_new(:,:)
    character(len=*), intent(in):: tag
    logical, intent(in):: comp_mag
    logical, intent(in):: report_id ! report identical variables
    logical, intent(in):: quiet

    ! Variables local to the subprogram:

    logical zero(size(data_old,1), size(data_old,2))

    double precision rel_diff(size(data_old,1), size(data_old,2))
    ! (absolute value of relative difference)

    double precision abs_diff(size(data_old,1), size(data_old,2))
    ! (absolute value of absolute difference)

    integer location(2)
    character(len=len(dashes)+len(tag)+20) tag_fmt

    !------------------------------------------------------

    call assert(shape(data_old) == shape(data_new), &
         "compare2_dble: shapes differ -- "  // tag)
    if (quiet) then
       tag_fmt = '(a, ":")'
    else
       tag_fmt = '(/, "' // dashes // '", /, a, ":")'
    end if

    if (size(data_old) == 0) then
       print tag_fmt, tag
       print *, "Zero-sized array"
    else
       if (all(data_old == data_new)) then
          if (report_id) then
             write(unit=*, fmt=tag_fmt, advance="no") tag
             print *, "arrays are identical."
          end if
       else
          if (quiet) then
             write(unit=*, fmt=tag_fmt, advance="no") tag
             print *, "arrays are different."
          else
             zero = data_old == 0d0 .or. data_new == 0d0
             where (.not. zero) rel_diff = abs(data_new / data_old - 1d0)
             abs_diff = abs(data_new - data_old)

             print tag_fmt, tag

             if (comp_mag) then
                print *
                print '("Average value of log10(abs(data_old)): ", 1pg8.1)', &
                     avg_mag(data_old)
             end if

             if (any(.not. zero)) then
                print *
                print *, 'Relative difference for non-zero values:'
                location = maxloc(rel_diff, mask=.not. zero)
                call prt_cmp(point(data_old, location), &
                     point(data_new, location), point(rel_diff, location), &
                     location)
             end if

             if (any(zero)) then
                print *
                print *, 'Absolute difference when there is a zero:'
                location = maxloc(abs_diff, mask=zero)
                call prt_cmp(point(data_old, location), &
                     point(data_new, location), point(abs_diff, location), &
                     location)
             end if

             print *
             print *, 'Absolute difference:'
             location = maxloc(abs_diff)
             call prt_cmp(point(data_old, location), &
                  point(data_new, location), point(abs_diff, location), &
                  location)
          end if
       end if
    end if

  end subroutine compare2_dble

  !***********************************************************

  subroutine compare3(data_old, data_new, tag, comp_mag, report_id, quiet)

    ! Rank 3, real

    use nr_util, only: assert
    use point_m, only: point

    real, intent(in):: data_old(:, :, :), data_new(:, : ,:)
    character(len=*), intent(in):: tag
    logical, intent(in):: comp_mag
    logical, intent(in):: report_id ! report identical variables
    logical, intent(in):: quiet

    ! Variables local to the subprogram:

    logical zero(size(data_old,1), size(data_old,2), size(data_old,3))

    real rel_diff(size(data_old,1), size(data_old,2), size(data_old,3))
    ! (absolute value of relative difference)

    real abs_diff(size(data_old,1), size(data_old,2), size(data_old,3))
    ! (absolute value of absolute difference)

    integer location(3)
    character(len=len(dashes)+len(tag)+20) tag_fmt

    !------------------------------------------------------

    call assert(shape(data_old) == shape(data_new), &
         "compare3: shapes differ -- " // tag)
    if (quiet) then
       tag_fmt = '(a, ":")'
    else
       tag_fmt = '(/, "' // dashes // '", /, a, ":")'
    end if

    if (size(data_old) == 0) then
       print tag_fmt, tag
       print *, "Zero-sized array"
    else
       if (all(data_old == data_new)) then
          if (report_id) then
             write(unit=*, fmt=tag_fmt, advance="no") tag
             print *, "arrays are identical."
          end if
       else
          if (quiet) then
             write(unit=*, fmt=tag_fmt, advance="no") tag
             print *, "arrays are different."
          else
             zero = data_old == 0. .or. data_new == 0.
             where (.not. zero) rel_diff = abs(data_new / data_old - 1.)
             abs_diff = abs(data_new - data_old)

             print tag_fmt, tag

             if (comp_mag) then
                print *
                print '("Average value of log10(abs(data_old)): ", 1pg8.1)', &
                     avg_mag(data_old)
             end if

             if (any(.not. zero)) then
                print *
                print *, 'Relative difference for non-zero values:'
                location = maxloc(rel_diff, mask=.not. zero)
                call prt_cmp(point(data_old, location), &
                     point(data_new, location), point(rel_diff, location), &
                     location)
             end if

             if (any(zero)) then
                print *
                print *, 'Absolute difference when there is a zero:'
                location = maxloc(abs_diff, mask=zero)
                call prt_cmp(point(data_old, location), &
                     point(data_new, location), point(abs_diff, location), &
                     location)
             end if

             print *
             print *, 'Absolute difference:'
             location = maxloc(abs_diff)
             call prt_cmp(point(data_old, location), &
                  point(data_new, location), point(abs_diff, location), &
                  location)
          end if
       end if
    end if

  end subroutine compare3

  !***********************************************************

  subroutine compare3_dble(data_old, data_new, tag, comp_mag, report_id, quiet)

    ! Rank 3, double precision

    use nr_util, only: assert
    use point_m, only: point

    double precision, intent(in):: data_old(:, :, :), data_new(:, : ,:)
    character(len=*), intent(in):: tag
    logical, intent(in):: comp_mag
    logical, intent(in):: report_id ! report identical variables
    logical, intent(in):: quiet

    ! Variables local to the subprogram:

    logical zero(size(data_old,1), size(data_old,2), size(data_old,3))

    double precision rel_diff(size(data_old,1), size(data_old,2), &
         size(data_old,3))
    ! (absolute value of relative difference)

    double precision abs_diff(size(data_old,1), size(data_old,2), &
         size(data_old,3))
    ! (absolute value of absolute difference)

    integer location(3)
    character(len=len(dashes)+len(tag)+20) tag_fmt

    !------------------------------------------------------

    call assert(shape(data_old) == shape(data_new), &
         "compare3_dble: shapes differ -- " // tag)
    if (quiet) then
       tag_fmt = '(a, ":")'
    else
       tag_fmt = '(/, "' // dashes // '", /, a, ":")'
    end if

    if (size(data_old) == 0) then
       print tag_fmt, tag
       print *, "Zero-sized array"
    else
       if (all(data_old == data_new)) then
          if (report_id) then
             write(unit=*, fmt=tag_fmt, advance="no") tag
             print *, "arrays are identical."
          end if
       else
          if (quiet) then
             write(unit=*, fmt=tag_fmt, advance="no") tag
             print *, "arrays are different."
          else
             zero = data_old == 0d0 .or. data_new == 0d0
             where (.not. zero) rel_diff = abs(data_new / data_old - 1d0)
             abs_diff = abs(data_new - data_old)

             print tag_fmt, tag

             if (comp_mag) then
                print *
                print '("Average value of log10(abs(data_old)): ", 1pg8.1)', &
                     avg_mag(data_old)
             end if

             if (any(.not. zero)) then
                print *
                print *, 'Relative difference for non-zero values:'
                location = maxloc(rel_diff, mask=.not. zero)
                call prt_cmp(point(data_old, location), &
                     point(data_new, location), point(rel_diff, location), &
                     location)
             end if

             if (any(zero)) then
                print *
                print *, 'Absolute difference when there is a zero:'
                location = maxloc(abs_diff, mask=zero)
                call prt_cmp(point(data_old, location), &
                     point(data_new, location), point(abs_diff, location), &
                     location)
             end if

             print *
             print *, 'Absolute difference:'
             location = maxloc(abs_diff)
             call prt_cmp(point(data_old, location), &
                  point(data_new, location), point(abs_diff, location), &
                  location)
          end if
       end if
    end if

  end subroutine compare3_dble

  !***********************************************************

  subroutine compare4(data_old, data_new, tag, comp_mag, report_id, quiet)

    ! Rank 4, real

    use nr_util, only: assert
    use point_m, only: point

    real, intent(in):: data_old(:, :, :, :), data_new(:, : ,:, :)
    character(len=*), intent(in):: tag
    logical, intent(in):: comp_mag
    logical, intent(in):: report_id ! report identical variables
    logical, intent(in):: quiet

    ! Variables local to the subprogram:

    logical zero(size(data_old,1), size(data_old,2), size(data_old,3), &
         size(data_old,4))

    real rel_diff(size(data_old,1), size(data_old,2), size(data_old,3), &
         size(data_old,4))
    ! (absolute value of relative difference)

    real abs_diff(size(data_old,1), size(data_old,2), size(data_old,3), &
         size(data_old,4))
    ! (absolute value of absolute difference)

    integer location(4)
    character(len=len(dashes)+len(tag)+20) tag_fmt

    !------------------------------------------------------

    call assert(shape(data_old) == shape(data_new), &
         "compare4: shapes differ -- " // tag)
    if (quiet) then
       tag_fmt = '(a, ":")'
    else
       tag_fmt = '(/, "' // dashes // '", /, a, ":")'
    end if

    if (size(data_old) == 0) then
       print tag_fmt, tag
       print *, "Zero-sized array"
    else
       if (all(data_old == data_new)) then
          if (report_id) then
             write(unit=*, fmt=tag_fmt, advance="no") tag
             print *, "arrays are identical."
          end if
       else
          if (quiet) then
             write(unit=*, fmt=tag_fmt, advance="no") tag
             print *, "arrays are different."
          else
             zero = data_old == 0. .or. data_new == 0.
             where (.not. zero) rel_diff = abs(data_new / data_old - 1.)
             abs_diff = abs(data_new - data_old)

             print tag_fmt, tag

             if (comp_mag) then
                print *
                print '("Average value of log10(abs(data_old)): ", 1pg8.1)', &
                     avg_mag(data_old)
             end if

             if (any(.not. zero)) then
                print *
                print *, 'Relative difference for non-zero values:'
                location = maxloc(rel_diff, mask=.not. zero)
                call prt_cmp(point(data_old, location), &
                     point(data_new, location), point(rel_diff, location), &
                     location)
             end if

             if (any(zero)) then
                print *
                print *, 'Absolute difference when there is a zero:'
                location = maxloc(abs_diff, mask=zero)
                call prt_cmp(point(data_old, location), &
                     point(data_new, location), point(abs_diff, location), &
                     location)
             end if

             print *
             print *, 'Absolute difference:'
             location = maxloc(abs_diff)
             call prt_cmp(point(data_old, location), &
                  point(data_new, location), point(abs_diff, location), &
                  location)
          end if
       end if
    end if

  end subroutine compare4

  !***********************************************************

  subroutine compare4_dble(data_old, data_new, tag, comp_mag, report_id, quiet)

    ! Rank 4, double precision

    use nr_util, only: assert
    use point_m, only: point

    double precision, intent(in):: data_old(:, :, :, :), data_new(:, : ,:, :)
    character(len=*), intent(in):: tag
    logical, intent(in):: comp_mag
    logical, intent(in):: report_id ! report identical variables
    logical, intent(in):: quiet

    ! Variables local to the subprogram:

    logical zero(size(data_old,1), size(data_old,2), size(data_old,3), &
         size(data_old,4))

    double precision rel_diff(size(data_old,1), size(data_old,2), &
         size(data_old,3), size(data_old,4))
    ! (absolute value of relative difference)

    double precision abs_diff(size(data_old,1), size(data_old,2), &
         size(data_old,3), size(data_old,4))
    ! (absolute value of absolute difference)

    integer location(4)
    character(len=len(dashes)+len(tag)+20) tag_fmt

    !------------------------------------------------------

    call assert(shape(data_old) == shape(data_new), &
         "compare4_dble: shapes differ -- " // tag)
    if (quiet) then
       tag_fmt = '(a, ":")'
    else
       tag_fmt = '(/, "' // dashes // '", /, a, ":")'
    end if

    if (size(data_old) == 0) then
       print tag_fmt, tag
       print *, "Zero-sized array"
    else
       if (all(data_old == data_new)) then
          if (report_id) then
             write(unit=*, fmt=tag_fmt, advance="no") tag
             print *, "arrays are identical."
          end if
       else
          if (quiet) then
             write(unit=*, fmt=tag_fmt, advance="no") tag
             print *, "arrays are different."
          else
             zero = data_old == 0d0 .or. data_new == 0d0
             where (.not. zero) rel_diff = abs(data_new / data_old - 1d0)
             abs_diff = abs(data_new - data_old)

             print tag_fmt, tag

             if (comp_mag) then
                print *
                print '("Average value of log10(abs(data_old)): ", 1pg8.1)', &
                     avg_mag(data_old)
             end if

             if (any(.not. zero)) then
                print *
                print *, 'Relative difference for non-zero values:'
                location = maxloc(rel_diff, mask=.not. zero)
                call prt_cmp(point(data_old, location), &
                     point(data_new, location), point(rel_diff, location), &
                     location)
             end if

             if (any(zero)) then
                print *
                print *, 'Absolute difference when there is a zero:'
                location = maxloc(abs_diff, mask=zero)
                call prt_cmp(point(data_old, location), &
                     point(data_new, location), point(abs_diff, location), &
                     location)
             end if

             print *
             print *, 'Absolute difference:'
             location = maxloc(abs_diff)
             call prt_cmp(point(data_old, location), &
                  point(data_new, location), point(abs_diff, location), &
                  location)
          end if
       end if
    end if

  end subroutine compare4_dble

end module compare_m
