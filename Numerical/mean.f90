module mean_m

  implicit none

contains

  pure function mean(array, dim, mask, missing)

    use, intrinsic:: ieee_arithmetic, only: ieee_value, IEEE_QUIET_NAN

    real, intent(in):: array(:, :)
    integer, intent(in):: dim
    logical, intent(in):: mask(:, :)
    real, intent(in):: missing
    real mean(size(array, 3 - dim))

    ! Local:
    integer, allocatable:: count_mask(:)

    !-------------------------------------------------------------------

    count_mask = count(mask, dim)

    where (count_mask /= 0)
       mean = sum(array, dim, mask) / count_mask
    elsewhere
       mean = missing
    end where

  end function mean

end module mean_m
