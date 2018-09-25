module DTRIDGL_m

  implicit none

contains

  SUBROUTINE DTRIDGL(L,AF,BF,CF,DF,XK)

    ! Double precision version of tridgl. This subroutine solves a
    ! system of tridiagional matrix equations. The form of the
    ! equations are:
    ! A(I)*X(I-1) + B(I)*X(I) + C(I)*X(I+1) = D(I)
    ! where i=1,l. Reviewed -CP

    integer, intent(in):: l
    double precision, intent(in):: AF(2:) ! (2:L)
    double precision, intent(in):: BF(L),DF(L)
    double precision, intent(in):: CF(:) ! (L - 1)
    double precision, intent(out):: XK(L)

    ! Variables local to the procedure:

    double precision AS(2:l),DS(l),  xkb, x
    integer i

    !----------------------------

    AS(L) = AF(L)/BF(L)
    DS(L) = DF(L)/BF(L)
    i = 2
    
    DO
       X=1d0/(BF(L+1-I) - CF(L+1-I)*AS(L+2-I))
       DS(L+1-I)=(DF(L+1-I)-CF(L+1-I)*DS(L+2-I))*X
       if (i == l) exit
       AS(L+1-I)=AF(L+1-I)*X
       i = i + 1
    end DO
    
    XK(1)=DS(1)
    DO I=2,L
       XKB=XK(I-1)
       XK(I)=DS(I)-AS(I)*XKB
    end DO

  END SUBROUTINE DTRIDGL

end module DTRIDGL_m
