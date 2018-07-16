module unit_matrix_m

  implicit none

contains

  SUBROUTINE unit_matrix(mat)

    ! Sets matrix to be a unit matrix.
    
    USE nrtype, only: wp
    
    REAL(WP), DIMENSION(:,:), INTENT(OUT) :: mat

    ! Local:
    INTEGER i,n

    !--------------------------------------------------------
    
    n=min(size(mat,1),size(mat,2))
    mat(:,:)=0.0_wp

    do i=1,n
       mat(i,i)=1.0_wp
    end do

  END SUBROUTINE unit_matrix

end module unit_matrix_m
