module abort_m

  implicit none

contains

  subroutine abort

#ifdef HAVE_MPI
    use mpi_f08, only: mpi_comm_world, mpi_abort, mpi_initialized, mpi_finalized

    logical flag

    !--------------------------------------------------------------

    call mpi_initialized(flag)

    if (flag) then
       call mpi_finalized(flag)

       if (.not. flag) then
          CALL mpi_abort(mpi_comm_world, 1)
       else
          stop 1
       end if
    else
       stop 1
    end if
#else
    stop 1
#endif       

  end subroutine abort
  
end module abort_m
