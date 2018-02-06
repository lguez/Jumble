module opt_merge_m

  implicit none

contains

  integer function opt_merge(param, default)

    ! Analogous to the intrinsic procedure "merge" : merges an
    ! optional parameter and a default value depending on the
    ! presence of the optional parameter.

    integer, intent(in), optional:: param
    integer, intent(in):: default

    !--------------

    if (present(param)) then
       opt_merge = param
    else
       opt_merge = default
    end if

  end function opt_merge

end module opt_merge_m
