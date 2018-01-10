module DTRIDGL_m

  implicit none

contains

  SUBROUTINE DTRIDGL(L,AF,BF,CF,DF,XK)

    ! Double precision version of tridgl. This subroutine solves a
    ! system of tridiagional matrix equations. The form of the
    ! equations are:
    ! A(I)*X(I-1) + B(I)*X(I) + C(I)*X(I+1) = D(I)
    ! where i=1,l less than 103. Reviewed -CP

    integer, intent(in):: l
    double precision, intent(in):: AF(L),BF(L),CF(L),DF(L)
    double precision, intent(out):: XK(L)

    ! Variables local to the procedure:

    integer, PARAMETER:: NMAX=201
    double precision AS(NMAX),DS(NMAX),  xkb, x
    integer i

    !----------------------------

    AS(L) = AF(L)/BF(L)
    DS(L) = DF(L)/BF(L)
    DO I=2,L
       X=1d0/(BF(L+1-I) - CF(L+1-I)*AS(L+2-I))
       AS(L+1-I)=AF(L+1-I)*X
       DS(L+1-I)=(DF(L+1-I)-CF(L+1-I)*DS(L+2-I))*X
    end DO
    XK(1)=DS(1)
    DO I=2,L
       XKB=XK(I-1)
       XK(I)=DS(I)-AS(I)*XKB
    end DO

  END SUBROUTINE DTRIDGL

end module DTRIDGL_m
