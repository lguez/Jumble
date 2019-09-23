module argwhere_m

  implicit none

  interface argwhere
    ! Returns the indices of true elements, in array element
    ! order. (The name of this procedure is taken from Numpy.) See
    ! also pack_indices.

     module procedure argwhere_1, argwhere_2
  end interface argwhere

contains

  pure function argwhere_1(mask)

    logical, intent(in):: mask(:)
    integer, allocatable:: argwhere_1(:)

    ! Local:
    integer i

    !--------------------------------------------------------------

    argwhere_1 = pack([(i, i = 1, size(mask))], mask)

  end function argwhere_1

  !******************************************************************  

  pure function argwhere_2(mask)

    logical, intent(in):: mask(:, :)
    integer, allocatable:: argwhere_2(:, :) ! (2, :)

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

    argwhere_2 = t(:, :n_packed)

  end function argwhere_2

end module argwhere_m
