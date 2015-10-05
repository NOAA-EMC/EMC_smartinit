!----------------------------------------------------------------------
      subroutine wndadj(validpt,u,v,htopo,dx,dy,im,jm,gdin)
!----------------------------------------------------------------------

! --- FROM WOCSS (Wind Over Complex Terrain Streamlines)
! --- Code received from Gene Petresku, AK
! --- Code adapted from Subroutine BAL5 (Annette Gibbs, January 2015)

! References

! Ludwig, F. L., J. M. Livingston, and R. M. Endlich, 1991: "Use of Mass 
!   Conservation and Dividing Streamline Concepts for Efficient Objective 
!   Analysis of Winds in Complex Terrain," J. Appl. Meteorol., 30,
!   1490-1499.

! Ludwig, F. L. and D. Sinton, 2000; Evaluating an Objective Wind Analysis 
!   Technique with a Long Record of Routinely Collected Data, J. Appl. 
!   Meteorol., 39, 335-348.

! Ludwig, F. L. and R. L. Street, 1995; Modification of Multiresolution
!   Feature Analysis for Application to Three-Dimensional Atmospheric Wind
!   Fields, J.  Atmos. Sci., 52, 139-157.

! Ludwig, F. L., R. L. Street, J. M. Schneider and K. R. Costigan, 1996: 
!   Analysis of Small-Scale Patterns of Atmospheric Motion in a Sheared, 
!   Convective Boundary Layer, J. Geophys. Res. (Atmospheres), 101D, 
!   9391-9411.

!     This routine adjusts winds towards nondivergence and causes flow around
!     the local topography.
!
!     INPUTS:  U (R ARRAY)     - GRIDDED X-DIRECTION WIND COMPONENTS
!              V (R ARRAY)     - GRIDDED Y-DIRECTION WIND COMPONENTS
!              HTOPO (R ARRAY) - GRIDDED TERRAIN HEIGHTS
!              ZSFC (R ARRAY)  - MODEL LEVELS HEIGHTS
!              NITER           - Upper limit on number of iterations in wndadj towards
!                                nondivergence

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
      REAL, ALLOCATABLE   :: usave(:,:), vsave(:,:)
      REAL, ALLOCATABLE   :: u1(:,:), v1(:,:)
      REAL, ALLOCATABLE    :: diffu(:,:), diffv(:,:)
      REAL, ALLOCATABLE   :: di(:,:)
      INTEGER niter,it
      REAL dxs,dys,ra,dxi,dyi,ddij
      LOGICAL normalize

      KK = 1
      NX=IM;NY=JM

      ALLOCATE (usave(NX,NY),STAT=kret)
      ALLOCATE (vsave(NX,NY),STAT=kret)
      ALLOCATE (u1(NX,NY),STAT=kret)
      ALLOCATE (v1(NX,NY),STAT=kret)
      ALLOCATE (diffu(NX,NY),STAT=kret)
      ALLOCATE (diffv(NX,NY),STAT=kret)
      ALLOCATE (di(NX,NY),STAT=kret)

      print *,'============================================================'
      print *,'WNDADJ:  DX  DY  NX NY', DX,DY,NX,NY,stat
      print *,'U ', MINVAL(U),MAXVAL(U)
      print *,'V ', MINVAL(V),MAXVAL(V)

! DDIJ is zero to produce nondivergent winds; RA - relaxation factor
! Niter - number of iterations towards nondivergence

      normalize=.FALSE.
      ddij=0.0
      niter=20
      ra=0.7

! Divergence is scaled to units of 10e-6/second.
! Use grid spacing in 100's of km.  DX/DY are in meters.
      
!     dxs=dx*1.0e-5
!     dys=dy*1.0e-5
! use 2 dx since not MM5
      dxs=2.0*dx*1.0e-5
      dys=2.0*dy*1.0e-5


      dxi=10.0/dxs
      dyi=10.0/dys

! Compute divergence (di=due+dvn) from flux differences between V components
! at north (vno) & south (vso) sides of box & U components at east (ue) &
! west (uw) sides of box.  DDIJ is zero to produce nondivergent winds. 
! RA - relaxation factor

      ddij=0.0

! save values at start of iteration
      do j=1,ny-1
      do i=1,nx-1
        usave(i,j)=u(i,j)
        vsave(i,j)=v(i,j)
      enddo
      enddo

      do it=1,niter

      print*,'iteration #=',it
      ij=0
      ijc=0
!     do j=1,ny-1
!     do i=1,nx-1
!      if(validpt(i,j).and. validpt(i+1,j).and.validpt(I+1,j+1).and.validpt(i,j+1))then
      do j=2,ny-1
      do i=2,nx-1
       if(validpt(i,j).and.validpt(i+1,j).and.validpt(I-1,j).and.validpt(i,j+1).and.validpt(i,j-1))then
!        UE=0.5*(U(I+1,J)+U(I+1,J+1))
!        UW=0.5*(U(I,J)  +U(I,J+1))
!        VSO=0.5*(V(I+1,J)+V(I,J))
!        VNO=0.5*(V(I,J+1)+V(I+1,J+1))
         UE=U(I+1,J)
         UW=U(I-1,J)
         VSO=V(I,J-1)
         VNO=V(I,J+1)
         DUE=dxi*(UE-UW)
         DVN=dyi*(VNO-VSO)
         DI(I,J)=DUE+DVN
         CUIJ=0.05*dxs*(DDIJ-DI(I,J))*RA
         CVIJ=0.05*dys*(DDIJ-DI(I,J))*RA
! remove 1/2 because not converting from cross pt to dot pt
!        CUIJ=0.1*dxs*(DDIJ-DI(I,J))*RA
!        CVIJ=0.1*dys*(DDIJ-DI(I,J))*RA
!  LIMIT CHANGES TO LESS THAN 1 FOR NUMERICAL STABILITY.
 
!        IF (abs(CUIJ) .gT. 1.0)print*,'i,j,it,cuij=',i,j,it,cuij
!        IF (abs(cvIJ) .gT. 1.0)print*,'i,j,it,cvij=',i,j,it,cvij

         IF (CUIJ .LT.-1.0) CUIJ=-1.0
         IF (CUIJ .GT. 1.0) CUIJ=1.0
         IF (CVIJ .LT.-1.0) CVIJ=-1.0
         IF (CVIJ .GT. 1.0) CVIJ=1.0
 
!        U(I+1,J)=U(I+1,J)+CUIJ
!        U(I+1,J+1)=U(I+1,J+1) +CUIJ
!        U(I,J)=U(I,J) -CUIJ
!        U(I,J+1)=U(I,J+1) -CUIJ
!        V(I+1,J)=V(I+1,J)-CVIJ
!        V(I,J)=V(I,J)-CVIJ
!        V(I,J+1)=V(I,J+1)+CVIJ
!        V(I+1,J+1)=V(I+1,J+1)+CVIJ
         U(I+1,J)=U(I+1,J)+CUIJ
         U(I-1,J)=U(I-1,J)-CUIJ
         V(I,J+1)=V(I,J+1)+CVIJ
         V(I,J-1)=V(I,J-1)-CVIJ
         ijc=ijc+1
       else
         ij=ij+1
       endif
      enddo
      enddo

      print *,'iteration=', it, 'total number of pts calculated', ijc
      print *,'iteration=', it, 'total number of pts not calculated', ij
      enddo

      where(validpt)
        diffu=U-usave
        diffv=V-vsave
      endwhere

      iu=0; iv=0
      do j=1,ny-1
      do i=1,nx-1
      if (validpt(i,j) .and. abs(diffu(i,j)).gt.10. ) then
        print *, i,j,'DIFFU,DIFFV', diffu(i,j),diffv(i,j),'U ',usave(i,j), U(I,J),'MDL Topo',zsfc(i,j),'NDFD Topo',HTOPO(i,j)
        iu=iu+1
      endif
      if (validpt(i,j) .and. abs(diffv(i,j)).gt.10.)  then
        print *, i,j,'DIFFU,DIFFV', diffu(i,j),diffv(i,j),'V ',vsave(i,j), V(I,J),'MDL Topo',zsfc(i,j),'NDFD Topo',HTOPO(i,j)
        iv=iv+1
      endif
      enddo
      enddo
      
      print *,'total number (diffu) > 10', iu
      print *,'total number (diffv) > 10', iv

      iu=0; iv=0
      do j=1,ny-1
      do i=1,nx-1
      if (validpt(i,j) .and. abs(diffu(i,j)).gt.5. ) then
        print *, i,j,'DIFFU,DIFFV', diffu(i,j),diffv(i,j),'U ',usave(i,j), U(I,J),'MDL Topo',zsfc(i,j),'NDFD Topo',HTOPO(i,j)
        iu=iu+1
      endif
      if (validpt(i,j) .and. abs(diffv(i,j)).gt.5.)  then
        print *, i,j,'DIFFU,DIFFV', diffu(i,j),diffv(i,j),'V ',vsave(i,j), V(I,J),'MDL Topo',zsfc(i,j),'NDFD Topo',HTOPO(i,j)
        iv=iv+1
      endif
      enddo
      enddo
      
      print *,'total number (diffu) > 5', iu
      print *,'total number (diffv) > 5', iv

      print *,'WNDADJ UWND  :',  MINVAL(U(:,:)),MAXVAL(U(:,:))
      print *,'WNDADJ VWND  :',  MINVAL(V(:,:)),MAXVAL(V(:,:))
      print *,'WNDADJ DIFF (new-old) UWND  :',  MINVAL(diffu(:,:)),MAXVAL(diffu(:,:))
      print *,'WNDADJ DIFF (new-old) VWND  :',  MINVAL(diffv(:,:)),MAXVAL(diffv(:,:))

      if(normalize)then
      sum1=0; sum2=0; q1=0
      do j=1,ny-1
      do i=1,nx-1
      if(validpt(i,j))then
        u1(i,j)=usave(i,j)-u(i,j)
        v1(i,j)=vsave(i,j)-v(i,j)
        q1=q1+1.0
        sum1=sum1+u1(i,j)
        sum2=sum2+v1(i,j)
      endif
      enddo
      enddo

      sum1=sum1/q1
      sum2=sum2/q1
      print*,'sum1,sum2,q1=',sum1,sum2,q1

      do j=1,ny-1
      do i=1,nx-1
      if(validpt(i,j))then
          u(i,j)=u1(i,j)+sum1
          v(i,j)=v1(i,j)+sum2
      endif
      enddo
      enddo

      diffu=U-usave
      diffv=V-vsave

      iu=0; iv=0
      do j=1,ny-1
      do i=1,nx-1
      if (abs(diffu(i,j)).gt.10. ) then
        print *, i,j,'DIFFU,DIFFV', diffu(i,j),diffv(i,j),'U ',usave(i,j), U(I,J),'MDL Topo',zsfc(i,j),'NDFD Topo',HTOPO(i,j)
        iu=iu+1
      endif
      if (abs(diffv(i,j)).gt.10.)  then
        print *, i,j,'DIFFU,DIFFV', diffu(i,j),diffv(i,j),'V ',vsave(i,j), V(I,J),'MDL Topo',zsfc(i,j),'NDFD Topo',HTOPO(i,j)
        iv=iv+1
      endif
      enddo
      enddo
      
      print *,'total number (diffu) > 10', iu
      print *,'total number (diffv) > 10', iv
      print *,'WNDADJ UWND  :',  MINVAL(U(:,:)),MAXVAL(U(:,:))
      print *,'WNDADJ VWND  :',  MINVAL(V(:,:)),MAXVAL(V(:,:))
      print *,'WNDADJ DIFF (new-old) UWND  :',  MINVAL(diffu(:,:)),MAXVAL(diffu(:,:))
      print *,'WNDADJ DIFF (new-old) VWND  :',  MINVAL(diffv(:,:)),MAXVAL(diffv(:,:))

      endif
      RETURN
      END
