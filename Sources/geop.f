MODULE geop_m

  IMPLICIT NONE

  INTERFACE geop
     MODULE PROCEDURE geop_r, geop_d, geop_i, geop_c, geop_dv
  END INTERFACE

  INTEGER, PARAMETER, private :: NPAR_GEOP=4, NPAR2_GEOP=2
  private geop_r, geop_d, geop_i, geop_c, geop_dv

CONTAINS

  FUNCTION geop_r(first,factor,n)
    REAL, INTENT(IN) :: first,factor
    INTEGER, INTENT(IN) :: n
    REAL, DIMENSION(n) :: geop_r
    INTEGER :: k,k2
    REAL :: temp
    if (n > 0) geop_r(1)=first
    if (n <= NPAR_GEOP) then
       do k=2,n
          geop_r(k)=geop_r(k-1)*factor
       end do
    else
       do k=2,NPAR2_GEOP
          geop_r(k)=geop_r(k-1)*factor
       end do
       temp=factor**NPAR2_GEOP
       k=NPAR2_GEOP
       do
          if (k >= n) exit
          k2=k+k
          geop_r(k+1:min(k2,n))=temp*geop_r(1:min(k,n-k))
          temp=temp*temp
          k=k2
       end do
    end if
  END FUNCTION geop_r

  FUNCTION geop_d(first,factor,n)
    DOUBLE PRECISION, INTENT(IN) :: first,factor
    INTEGER, INTENT(IN) :: n
    DOUBLE PRECISION, DIMENSION(n) :: geop_d
    INTEGER :: k,k2
    DOUBLE PRECISION :: temp
    if (n > 0) geop_d(1)=first
    if (n <= NPAR_GEOP) then
       do k=2,n
          geop_d(k)=geop_d(k-1)*factor
       end do
    else
       do k=2,NPAR2_GEOP
          geop_d(k)=geop_d(k-1)*factor
       end do
       temp=factor**NPAR2_GEOP
       k=NPAR2_GEOP
       do
          if (k >= n) exit
          k2=k+k
          geop_d(k+1:min(k2,n))=temp*geop_d(1:min(k,n-k))
          temp=temp*temp
          k=k2
       end do
    end if
  END FUNCTION geop_d

  FUNCTION geop_i(first,factor,n)
    INTEGER, INTENT(IN) :: first,factor,n
    INTEGER, DIMENSION(n) :: geop_i
    INTEGER :: k,k2,temp
    if (n > 0) geop_i(1)=first
    if (n <= NPAR_GEOP) then
       do k=2,n
          geop_i(k)=geop_i(k-1)*factor
       end do
    else
       do k=2,NPAR2_GEOP
          geop_i(k)=geop_i(k-1)*factor
       end do
       temp=factor**NPAR2_GEOP
       k=NPAR2_GEOP
       do
          if (k >= n) exit
          k2=k+k
          geop_i(k+1:min(k2,n))=temp*geop_i(1:min(k,n-k))
          temp=temp*temp
          k=k2
       end do
    end if
  END FUNCTION geop_i

  FUNCTION geop_c(first,factor,n)
    COMPLEX, INTENT(IN) :: first,factor
    INTEGER, INTENT(IN) :: n
    COMPLEX, DIMENSION(n) :: geop_c
    INTEGER :: k,k2
    COMPLEX :: temp
    if (n > 0) geop_c(1)=first
    if (n <= NPAR_GEOP) then
       do k=2,n
          geop_c(k)=geop_c(k-1)*factor
       end do
    else
       do k=2,NPAR2_GEOP
          geop_c(k)=geop_c(k-1)*factor
       end do
       temp=factor**NPAR2_GEOP
       k=NPAR2_GEOP
       do
          if (k >= n) exit
          k2=k+k
          geop_c(k+1:min(k2,n))=temp*geop_c(1:min(k,n-k))
          temp=temp*temp
          k=k2
       end do
    end if
  END FUNCTION geop_c

  FUNCTION geop_dv(first,factor,n)
    DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: first,factor
    INTEGER, INTENT(IN) :: n
    DOUBLE PRECISION, DIMENSION(size(first),n) :: geop_dv
    INTEGER :: k,k2
    DOUBLE PRECISION, DIMENSION(size(first)) :: temp
    if (n > 0) geop_dv(:,1)=first(:)
    if (n <= NPAR_GEOP) then
       do k=2,n
          geop_dv(:,k)=geop_dv(:,k-1)*factor(:)
       end do
    else
       do k=2,NPAR2_GEOP
          geop_dv(:,k)=geop_dv(:,k-1)*factor(:)
       end do
       temp=factor**NPAR2_GEOP
       k=NPAR2_GEOP
       do
          if (k >= n) exit
          k2=k+k
          geop_dv(:,k+1:min(k2,n))=geop_dv(:,1:min(k,n-k))*&
               spread(temp,2,size(geop_dv(:,1:min(k,n-k)),2))
          temp=temp*temp
          k=k2
       end do
    end if
  END FUNCTION geop_dv

END MODULE geop_m
