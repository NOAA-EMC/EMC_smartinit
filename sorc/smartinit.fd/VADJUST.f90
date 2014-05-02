!----------------------------------------------------------------------
      subroutine vadjust(VALIDPT,U,V,HTOPO,DX,DY,IM,JM,gdin)
!----------------------------------------------------------------------

! --- FROM CALMET   Version: 5.8        Level: 050328                 ADJUST

!      THIS ROUTINE ADJUSTS SURFACE WINDS FOR TERRAIN EFFECTS
!     INPUTS:  U (R ARRAY)     - GRIDDED X-DIRECTION WIND COMPONENTS
!              V (R ARRAY)     - GRIDDED Y-DIRECTION WIND COMPONENTS
!              HTOPO (R ARRAY) - GRIDDED TERRAIN HEIGHTS
!              HBAR (R ARRAY)  - MODEL LEVELS HEIGHTS
!              UB (R ARRAY)    - U-COMPONENT BOUNDARY VALUES
!              VB (R ARRAY)    - V-COMPONENT BOUNDARY VALUES
!       Parameters: MXNX, MXNY, MXNZ, MXNZP1

!     OUTPUTS:  U (R ARRAY) - X-DIRECTION WIND COMPONENTS WITH
!                             ADJUSTED SURFACE LAYER WINDS
!               V (R ARRAY) - Y-DIRECTION WIND COMPONENTS WITH
!                             ADJUSTED SURFACE LAYER WINDS

    use constants
    use grddef
    use aset2d
    use aset3d

    LOGICAL, INTENT(IN) :: VALIDPT(:,:)
    REAL, INTENT(INOUT) :: U(:,:),V(:,:)
    REAL, INTENT(IN) :: HTOPO(:,:),DX,DY
    TYPE (GINFO)        :: GDIN
    REAL, ALLOCATABLE   :: UB(:,:),VB(:,:)
    REAL, ALLOCATABLE   :: PHI(:,:,:)
    real HBAR,DXI,DYI,FX,FY,HTOIM1,HTOJM1,HTOIP1,HTOJP1,DHDX,DHDY, &
         DXSQ,DYSQ,DSQ,FACT,ERROR,ERR,EPSI,OVREL,XX,YY
    integer itmax,ii,jj,kk,idir,it

!     ITERATION CRITERIA

      DATA ITMAX,EPSI,OVREL/75,0.02,1.5/
      NX=IM;NY=JM
      KK = 1
      print *,'VADJUST:  DX  DY  NX NY', DX,DY,NX,NY
      ALLOCATE (UB(NY,2),VB(NX,2),STAT=kret)
      ALLOCATE (PHI(NX,NY,2),STAT=kret)

!     COMPUTE TERRAIN GRADIENTS AND INITIAL POTENTIAL
      UB=0.1;VB=0.1
      DXI=0.5/DX
      DYI=0.5/DY
      do j=1,ny
      do i=1,nx
         HBAR=HGHT(I,J,1)
         FX=DXI/(HBAR)
         FY=DYI/(HBAR)
         HTOIM1=HTOPO(I,J)
         HTOJM1=HTOPO(I,J)
         IF(I.GT.1) HTOIM1=HTOPO(I-1,J)
         IF(J.GT.1) HTOJM1=HTOPO(I,J-1)
         HTOIP1=HTOPO(I,J)
         HTOJP1=HTOPO(I,J)
         IF(I.LT.NX) HTOIP1=HTOPO(I+1,J)
         IF(J.LT.NY) HTOJP1=HTOPO(I,J+1)
         DHDX=(HTOIP1-HTOIM1)*FX
         DHDY=(HTOJP1-HTOJM1)*FY
         if (validpt(i,j)) then 
           PHI(I,J,2)=(U(I,J)*DHDX+V(I,J)*DHDY)

!     CALCULATE THE VERTICAL VELOCITY DUE TO TOPOGRAPHIC EFFECTS (JTM)
           WTOPO=U(I,J)*DHDX+V(I,J)*DHDY
           PHI(I,J,1)=WTOPO
!          TEST assume terrain following winds
           PHI(I,J,1)=0.1
         else
          PHI(I,J,1)=0.1
          PHI(I,J,2)=0.1
        endif
      enddo
      enddo

!     SET BOUNDARY VALUES FOR PHI
      UB(:,1)=PHI(1,:,1)
      UB(:,2)=PHI(NX,:,1)
      VB(:,1)=PHI(:,1,1)
      VB(:,2)=PHI(:,NY,1)

      print *,'VADJUST PHI 1 :', MINVAL(PHI(:,:,1)),MAXVAL(PHI(:,:,1))
      print *,'VADJUST PHI 2 :', MINVAL(PHI(:,:,2)),MAXVAL(PHI(:,:,2))
      print *,'VADJUST UB  :', MINVAL(UB(:,1)),MAXVAL(UB(:,2))
      print *,'VADJUST VB  :', MINVAL(VB(:,1)),MAXVAL(VB(:,2))

!     SOLVE POISSON EQUATION BY GAUSS-SEIDEL METHOD FOR
!     VELOCITY POTENTIAL

      DXSQ=DX*DX
      DYSQ=DY*DY
      DSQ=DXSQ*DYSQ
      FACT=1.0/(2.0*(DXSQ+DYSQ))
      print *,'VADJUST DX  DY ', DX,DY,' FACT', FACT
      DO 100 IT=1,ITMAX
        DO 90 IDIR=1,4
          ERROR=-1.0E+09
          do jj=1,ny
          do ii=1,nx
            SELECT CASE (IDIR)
             CASE (1)
              I=II
              J=JJ
             CASE(2)  
              I=NX-II+1
              J=JJ
             CASE (3) 
              I=II
              J=NY-JJ+1
             CASE(4)
              I=NX-II+1
              J=NY-JJ+1
           END SELECT

           XOLD=PHI(I,J,KK)
           PHIIM1=UB(J,1)
           IF(I.GT.1)PHIIM1=PHI(I-1,J,KK)
           PHIJM1=VB(I,1)
           IF(J.GT.1)PHIJM1=PHI(I,J-1,KK)
           PHIIP1=UB(J,2)
           IF(I.LT.NX )PHIIP1=PHI(I+1,J,KK)
           PHIJP1=VB(I,2)
           IF(J.LT.NY)PHIJP1=PHI(I,J+1,KK)
           XX=DYSQ*(PHIIP1+PHIIM1)
           YY=DXSQ*(PHIJP1+PHIJM1)
           PHI(I,J,KK) = (1.-OVREL)*PHI(I,J,KK)+ OVREL*FACT*(XX+YY-DSQ*PHI(I,J,2))

           IF(I.EQ.1) UB(J,1)=PHI(1,J,KK)
           IF(I.EQ.NX) UB(J,2)=PHI(NX,J,KK)
           IF(J.EQ.1) VB(I,1)=PHI(I,1,KK)
           IF(J.EQ.NY) VB(I,2)=PHI(I,NY,KK)

           IF(ABS(XOLD).GE.1.0E-10) THEN     
             ERR=ABS((PHI(I,J,KK)-XOLD)/XOLD)
             ERROR=AMAX1(ERR,ERROR)
           ENDIF
         enddo
         enddo 
   90   CONTINUE
        print *,'VADJUST :  ERROR',IT, IDIR, ERROR,' EPSI',EPSI,' XOLD',PHIOLD
        IF (ERROR.LE.EPSI) exit
  100 CONTINUE

      print *,'VADJUST PHI 1 :POIS ', MINVAL(PHI(:,:,1)),MAXVAL(PHI(:,:,1))
      print *,'VADJUST PHI 2 :', MINVAL(PHI(:,:,2)),MAXVAL(PHI(:,:,2))
      print *,'VADJUST UB  :', MINVAL(UB(:,1)),MAXVAL(UB(:,2))
      print *,'VADJUST VB  :', MINVAL(VB(:,1)),MAXVAL(VB(:,2))

!     COMPUTE WIND COMPONENTS FROM VELOCITY POTENTIAL
      do j=2,ny-1
      do i=2,nx-1
        PHIIM1=UB(J,1)
        PHIJM1=VB(I,1)
        PHIIP1=UB(J,2)
        PHIJP1=VB(I,2)
        IF(I.GT.1) PHIIM1=PHI(I-1,J,KK)
        IF(J.GT.1) PHIJM1=PHI(I,J-1,KK)
        IF(I.LT.NX) PHIIP1=PHI(I+1,J,KK)
        IF(J.LT.NY) PHIJP1=PHI(I,J+1,KK)
        if (validpt(i-1,j).and.validpt(i+1,j).and.validpt(i,j-1).and.validpt(i,j+1).and. &
            validpt(i-1,j-1).and.validpt(i+1,j+1).and. validpt(i-1,j+1).and.  &
            validpt(i+1,j-1).and.validpt(i,j)) then
          UOLD=U(I,J)
          VOLD=V(I,J)
          U(I,J)=(PHIIP1-PHIIM1)*DXI+U(I,J)
          V(I,J)=(PHIJP1-PHIJM1)*DYI+V(I,J)
          diffi=UOLD-U(i,j)
          diffj=VOLD-V(i,j)
          if (abs(diffi).gt.10.) U(I,J)=UOLD
          if (abs(diffj).gt.10.) V(I,J)=VOLD
       endif
      enddo
      enddo
      print *,'VADJUST UWND  :',  MINVAL(U(:,:)),MAXVAL(U(:,:))
      print *,'VADJUST VWND  :',  MINVAL(V(:,:)),MAXVAL(V(:,:))

!     RESET BOUNDARY ARRAY
      UB=0.
      VB=0.
      RETURN
      END
