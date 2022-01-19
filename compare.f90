module compare_m

  use assert_m, only: assert
  use avg_mag_m, only: avg_mag
  use opt_merge_m, only: opt_merge
  use point_m, only: point
  use prt_cmp_m, only: prt_cmp

  implicit none

  interface compare
     ! Makes a numerical comparison between two arrays of rank 1 to 4,
     ! of type real or double precision.

     ! subroutine compare(data_old, data_new, tag, comp_mag, report_id, quiet, &
     !    valid, different_domains)
     ! real or double precision, intent(in), rank 1 to 4:: data_old, data_new
     ! character(len = *), intent(in):: tag
     ! logical, intent(in):: comp_mag
     ! logical, intent(in):: report_id  (report identical variables)

     ! logical, intent(in):: quiet
     ! Say if arrays are different but do not quantify difference

     ! logical, intent(in), same rank as data_old and data_new:: valid
     ! This is the mask (used for the two arrays, data_old and
     ! data_new) where comparison will be done.

     ! logical, intent(in), optional:: different_domains (default .false.)
     ! This tells whether data_old or data_new is defined on a larger domain
     ! than the one given by argument valid. Providing argument
     ! different_domains does not change numerical results, it is just
     ! used to output more informative error messages.

     module procedure compare1, compare1_dble, compare2, compare2_dble, &
          compare3, compare3_dble, compare4, compare4_dble
  end interface compare

  character(len = *), parameter:: dashes &
       = "---------------------------------------------"

  logical different_domains_not_opt

  private
  public compare

contains

  subroutine compare1(data_old, data_new, tag, comp_mag, report_id, quiet, &
       valid, different_domains)

    ! Rank 1, real

    real, intent(in):: data_old(:), data_new(:)
    character(len = *), intent(in):: tag
    logical, intent(in):: comp_mag, report_id, quiet
    logical, intent(in):: valid(:)
    logical, intent(in), optional:: different_domains

    ! Local:

    logical zero(size(data_old)), not_zero(size(data_old))

    real rel_diff(size(data_old))
    ! (absolute value of relative difference)

    real abs_diff(size(data_old))
    ! (absolute value of absolute difference)

    integer location(1)
    character(len = len(dashes) + len(tag) + 20) tag_fmt
    integer, parameter:: wp = kind(0.)

    !------------------------------------------------------

    include "compare.h"

  end subroutine compare1

  !***********************************************************

  subroutine compare1_dble(data_old, data_new, tag, comp_mag, report_id, &
       quiet, valid, different_domains)

    ! Rank 1, double precision

    double precision, intent(in):: data_old(:), data_new(:)
    character(len = *), intent(in):: tag
    logical, intent(in):: comp_mag, report_id, quiet
    logical, intent(in):: valid(:)
    logical, intent(in), optional:: different_domains

    ! Local:

    logical zero(size(data_old)), not_zero(size(data_old))

    double precision rel_diff(size(data_old))
    ! (absolute value of relative difference)

    double precision abs_diff(size(data_old))
    ! (absolute value of absolute difference)

    integer location(1)
    character(len = len(dashes) + len(tag) + 20) tag_fmt
    integer, parameter:: wp = kind(0d0)

    !------------------------------------------------------

    include "compare.h"

  end subroutine compare1_dble

  !***********************************************************

  subroutine compare2(data_old, data_new, tag, comp_mag, report_id, quiet, &
       valid, different_domains)

    ! Rank 2, real

    real, intent(in):: data_old(:,:), data_new(:,:)
    character(len = *), intent(in):: tag
    logical, intent(in):: comp_mag, report_id, quiet
    logical, intent(in):: valid(:, :)
    logical, intent(in), optional:: different_domains

    ! Local:

    logical, dimension(size(data_old,1), size(data_old,2))::  zero, not_zero

    real rel_diff(size(data_old,1), size(data_old,2))
    ! (absolute value of relative difference)

    real abs_diff(size(data_old,1), size(data_old,2))
    ! (absolute value of absolute difference)

    integer location(2)
    character(len = len(dashes) + len(tag) + 20) tag_fmt
    integer, parameter:: wp = kind(0.)

    !------------------------------------------------------

    include "compare.h"

  end subroutine compare2

  !***********************************************************

  subroutine compare2_dble(data_old, data_new, tag, comp_mag, report_id, &
       quiet, valid, different_domains)

    ! Rank 2, double precision

    double precision, intent(in):: data_old(:,:), data_new(:,:)
    character(len = *), intent(in):: tag
    logical, intent(in):: comp_mag, report_id, quiet
    logical, intent(in):: valid(:, :)
    logical, intent(in), optional:: different_domains

    ! Local:

    logical, dimension(size(data_old,1), size(data_old,2)):: zero, not_zero

    double precision rel_diff(size(data_old,1), size(data_old,2))
    ! (absolute value of relative difference)

    double precision abs_diff(size(data_old,1), size(data_old,2))
    ! (absolute value of absolute difference)

    integer location(2)
    character(len = len(dashes) + len(tag) + 20) tag_fmt
    integer, parameter:: wp = kind(0d0)

    !------------------------------------------------------

    include "compare.h"

  end subroutine compare2_dble

  !***********************************************************

  subroutine compare3(data_old, data_new, tag, comp_mag, report_id, quiet, &
       valid, different_domains)

    ! Rank 3, real

    real, intent(in):: data_old(:, :, :), data_new(:, : ,:)
    character(len = *), intent(in):: tag
    logical, intent(in):: comp_mag, report_id, quiet
    logical, intent(in):: valid(:, :, :)
    logical, intent(in), optional:: different_domains

    ! Local:

    logical, dimension(size(data_old,1), size(data_old,2), size(data_old,3)):: &
         zero, not_zero

    real rel_diff(size(data_old,1), size(data_old,2), size(data_old,3))
    ! (absolute value of relative difference)

    real abs_diff(size(data_old,1), size(data_old,2), size(data_old,3))
    ! (absolute value of absolute difference)

    integer location(3)
    character(len = len(dashes) + len(tag) + 20) tag_fmt
    integer, parameter:: wp = kind(0.)

    !------------------------------------------------------

    include "compare.h"

  end subroutine compare3

  !***********************************************************

  subroutine compare3_dble(data_old, data_new, tag, comp_mag, report_id, &
       quiet, valid, different_domains)

    ! Rank 3, double precision

    double precision, intent(in):: data_old(:, :, :), data_new(:, : ,:)
    character(len = *), intent(in):: tag
    logical, intent(in):: comp_mag, report_id, quiet
    logical, intent(in):: valid(:, :, :)
    logical, intent(in), optional:: different_domains

    ! Local:

    logical, dimension(size(data_old,1), size(data_old,2), size(data_old,3)):: &
         zero, not_zero

    double precision rel_diff(size(data_old,1), size(data_old,2), &
         size(data_old,3))
    ! (absolute value of relative difference)

    double precision abs_diff(size(data_old,1), size(data_old,2), &
         size(data_old,3))
    ! (absolute value of absolute difference)

    integer location(3)
    character(len = len(dashes) + len(tag) + 20) tag_fmt
    integer, parameter:: wp = kind(0d0)

    !------------------------------------------------------

    include "compare.h"

  end subroutine compare3_dble

  !***********************************************************

  subroutine compare4(data_old, data_new, tag, comp_mag, report_id, quiet, &
       valid, different_domains)

    ! Rank 4, real

    real, intent(in):: data_old(:, :, :, :), data_new(:, : ,:, :)
    character(len = *), intent(in):: tag
    logical, intent(in):: comp_mag, report_id, quiet
    logical, intent(in):: valid(:, :, :, :)
    logical, intent(in), optional:: different_domains

    ! Local:

    logical, dimension(size(data_old,1), size(data_old,2), size(data_old,3), &
         size(data_old,4)):: zero, not_zero

    real rel_diff(size(data_old,1), size(data_old,2), size(data_old,3), &
         size(data_old,4))
    ! (absolute value of relative difference)

    real abs_diff(size(data_old,1), size(data_old,2), size(data_old,3), &
         size(data_old,4))
    ! (absolute value of absolute difference)

    integer location(4)
    character(len = len(dashes) + len(tag) + 20) tag_fmt
    integer, parameter:: wp = kind(0.)

    !------------------------------------------------------

    include "compare.h"

  end subroutine compare4

  !***********************************************************

  subroutine compare4_dble(data_old, data_new, tag, comp_mag, report_id, &
       quiet, valid, different_domains)

    ! Rank 4, double precision

    double precision, intent(in):: data_old(:, :, :, :), data_new(:, : ,:, :)
    character(len = *), intent(in):: tag
    logical, intent(in):: comp_mag, report_id, quiet
    logical, intent(in):: valid(:, :, :, :)
    logical, intent(in), optional:: different_domains

    ! Local:

    logical, dimension(size(data_old,1), size(data_old,2), size(data_old,3), &
         size(data_old,4)):: zero, not_zero

    double precision rel_diff(size(data_old,1), size(data_old,2), &
         size(data_old,3), size(data_old,4))
    ! (absolute value of relative difference)

    double precision abs_diff(size(data_old,1), size(data_old,2), &
         size(data_old,3), size(data_old,4))
    ! (absolute value of absolute difference)

    integer location(4)
    character(len = len(dashes) + len(tag) + 20) tag_fmt
    integer, parameter:: wp = kind(0d0)

    !------------------------------------------------------

    include "compare.h"

  end subroutine compare4_dble

end module compare_m
