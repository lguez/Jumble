module unit_matrix_m

  implicit none

  interface unit_matrix
    ! Sets matrix to be a unit matrix.
    
     module procedure unit_matrix_s, unit_matrix_d
  end interface unit_matrix

  private unit_matrix_s, unit_matrix_d

contains

  SUBROUTINE unit_matrix_s(mat)

    REAL, DIMENSION(:,:), INTENT(OUT) :: mat

    ! Local:
    INTEGER i,n

    !--------------------------------------------------------
    
    n=min(size(mat,1),size(mat,2))
    mat(:,:)=0.

    do i=1,n
       mat(i,i)=1.
    end do

  END SUBROUTINE unit_matrix_s

  !***************************************************************

  SUBROUTINE unit_matrix_d(mat)

    double precision, DIMENSION(:,:), INTENT(OUT) :: mat

    ! Local:
    INTEGER i,n

    !--------------------------------------------------------
    
    n=min(size(mat,1),size(mat,2))
    mat(:,:)=0d0

    do i=1,n
       mat(i,i)=1d0
    end do

  END SUBROUTINE unit_matrix_d

end module unit_matrix_m
