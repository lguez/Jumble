module uniq_m

  implicit none

contains

  subroutine uniq(v, squeezed, multiplicity)

    ! This procedure mimicks the Unix utility uniq, with or without
    ! argument "-c". The procedure receives an array "v", compares
    ! successice elements, and writes one copy of each element to the
    ! output array "squeezed". The second and succeeding copies of
    ! repeated successive elements are not output. "v" can be
    ! zero-sized.

    integer, intent(in):: v(:)
    integer, pointer:: squeezed(:)
    integer, pointer, optional:: multiplicity(:)

    ! Local:
    logical new_val(size(v)) ! new value
    integer i, n, n_squeezed
    integer, allocatable:: i_new(:)
    
    !------------------------------------------------------------

    n = size(v)

    if (n == 0) then
       allocate(squeezed(0))
       if (present(multiplicity)) allocate(multiplicity(0))
    else
       new_val(1) = .true.
       forall (i = 2: n) new_val(i) = v(i) /= v(i - 1)

       n_squeezed = count(new_val)
       allocate(squeezed(n_squeezed))

       if (present(multiplicity)) then
          allocate(i_new(n_squeezed), multiplicity(n_squeezed))
          i_new = pack((/(i, i = 1, n)/), new_val)
          squeezed = v(i_new)

          forall (i = 1: n_squeezed - 1) &
               multiplicity(i) = i_new(i + 1) - i_new(i)
          multiplicity(n_squeezed) = n - i_new(n_squeezed) + 1
       else
          squeezed = pack(v, new_val)
       end if
    end if

  end subroutine uniq

end module uniq_m
