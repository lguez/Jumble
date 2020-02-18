module opt_merge_m

  implicit none

  interface opt_merge
    ! Analogous to the intrinsic procedure "merge" : merges an
    ! optional parameter and a default value depending on the
    ! presence of the optional parameter.

     module procedure opt_merge_integer, opt_merge_logical
  end interface opt_merge

  private
  public opt_merge

contains

  integer function opt_merge_integer(param, default)

    integer, intent(in), optional:: param
    integer, intent(in):: default

    !--------------

    if (present(param)) then
       opt_merge_integer = param
    else
       opt_merge_integer = default
    end if

  end function opt_merge_integer

  !*********************************************************************

  logical function opt_merge_logical(param, default)

    logical, intent(in), optional:: param
    logical, intent(in):: default

    !--------------

    if (present(param)) then
       opt_merge_logical = param
    else
       opt_merge_logical = default
    end if

  end function opt_merge_logical

end module opt_merge_m
