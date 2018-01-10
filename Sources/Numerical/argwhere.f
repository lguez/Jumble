module argwhere_m

  implicit none

contains

  pure function argwhere(mask)

    ! Returns the indices of true elements, in array element
    ! order. (The name of this procedure is taken from Numpy.) See
    ! also pack_indices.

    logical, intent(in):: mask(:, :)
    integer, allocatable:: argwhere(:, :) ! (2, :)

    ! Local:
    integer n_packed, i, j, m
    integer t(2, size(mask)) ! Maximum number of true elements is
                             ! size(mask).

    !--------------------------------------------------------------

    m = size(mask, 1)
    n_packed = 0

    do j = 1, size(mask, 2)
       do i = 1, m
          if (mask(i, j)) then
             n_packed = n_packed + 1
             t(:, n_packed) = [i, j]
          end if
       end do
    end do

    argwhere = t(:, :n_packed)

  end function argwhere

end module argwhere_m
