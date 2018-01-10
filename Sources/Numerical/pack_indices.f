module pack_indices_m

  implicit none

contains

  pure function pack_indices(my_array, excluded)

    ! Returns the indices of the elements not excluded in my_array.

    integer, allocatable:: pack_indices(:, :) ! (2, n_packed)
    integer, intent(in):: my_array(:, :) ! (m, :)
    integer, intent(in):: excluded(:) ! excluded values of my_array

    ! Local:
    integer n_packed, i, j, m
    integer t(2, size(my_array)) ! Maximum number of accepted elements
                                 ! is size(my_array).

    !---------------------------------------------------------------

    m = size(my_array, 1)
    n_packed = 0

    do j = 1, size(my_array, 2)
       do i = 1, m
          if (all(my_array(i, j) /= excluded)) then
             n_packed = n_packed + 1
             t(:, n_packed) = [i, j]
          end if
       end do
    end do

    pack_indices = t(:, :n_packed)

  end function pack_indices

end module pack_indices_m
