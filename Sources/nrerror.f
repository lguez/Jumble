module nrerror_m

  implicit none

contains

  SUBROUTINE nrerror(string)

    CHARACTER(LEN=*), INTENT(IN) :: string

    print *, 'nrerror: ', string
    print *, 'program terminated by nrerror'
    stop 1

  END SUBROUTINE nrerror

end module nrerror_m
