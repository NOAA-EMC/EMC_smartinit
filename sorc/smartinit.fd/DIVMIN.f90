!----------------------------------------------------------------------
      subroutine divmin(validpt,u,v,htopo,dx,dy,im,jm,gdin)
!----------------------------------------------------------------------

! --- From CALMET Version 5.8 Level: 050328, S. Douglas, SAI
! --- Code received from Jeff McQueen
! --- Code adapted from Subroutine MINIM  (Annette Gibbs, February 2015)

! References

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
      REAL, ALLOCATABLE   :: diffu(:,:), diffv(:,:)
      REAL, ALLOCATABLE   :: div(:,:)
      INTEGER niter,it
      REAL dxi,dyi

      INTERFACE
        SUBROUTINE divcel(validpt,u,v,div,nx,ny,dxm,dym,divmax)
!----------------------------------------------------------------------
        LOGICAL, INTENT(IN) :: VALIDPT(:,:)
        REAL, INTENT(IN) :: U(:,:),V(:,:)
        REAL, INTENT(INOUT) :: DIV(:,:)
        REAL, INTENT(IN) :: DXM,DYM
        INTEGER, INTENT(IN) :: NX,NY
        REAL dxi,dyi
        END SUBROUTINE divcel
      END INTERFACE

      KK=1
      NX=IM;NY=JM
      DXM=DX;DYM=DY

      ALLOCATE (usave(NX,NY),STAT=kret)
      ALLOCATE (vsave(NX,NY),STAT=kret)
      ALLOCATE (diffu(NX,NY),STAT=kret)
      ALLOCATE (diffv(NX,NY),STAT=kret)
      ALLOCATE (div(NX,NY),STAT=kret)

      print *,'============================================================'
      print *,'DIVMIN:  DXM  DYM  NX NY', DXM,DYM,NX,NY,stat
      print *,'U ', MINVAL(U),MAXVAL(U)
      print *,'V ', MINVAL(V),MAXVAL(V)

! Niter - number of iterations towards nondivergence
! Divlim - maximum divergence

      niter=50
      divmax=-1.0e+09
      divlim=5.e-6

! Save values at start of iteration

      do j=1,ny
      do i=1,nx
        usave(i,j)=u(i,j)
        vsave(i,j)=v(i,j)
      enddo
      enddo

! Calculate divergence

      call divcel(validpt,u,v,div,nx,ny,dxm,dym,divmax)

      alpha1=0.5
      alpha2=0.5
      alpha3=0.5
      alpha4=0.5
      al1234=alpha1+alpha2+alpha3+alpha4

! Adjust horizontal wind fields

      do 20 it=1,niter
        print*,'iteration #=',it
        ij=0
        ijc=0
        do 30 idir=1,4
          do jj=2,ny
          do ii=2,nx
            SELECT CASE (idir)
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

            ip1=i
            im1=i
            jp1=j
            jm1=j
            if(i.lt.nx)ip1=i+1
            if(i.gt.1)im1=i-1
            if(j.lt.ny)jp1=j+1
            if(j.gt.1)jm1=j-1

            if(validpt(i,j))then
              if(div(i,j) .ne. 0.)then

                uip1=u(i,j)
                uim1=u(i,j)
                vjp1=v(i,j)
                vjm1=v(i,j)
                if(validpt(ip1,j))uip1=u(ip1,j)
                if(validpt(im1,j))uim1=u(im1,j)
                if(validpt(i,jp1))vjp1=v(i,jp1)
                if(validpt(i,jm1))vjm1=v(i,jm1)

! Make adjustment

                ut=-2.*(div(i,j)*dxm)/al1234
                vt=-2.*(div(i,j)*dym)/al1234
        
!               IF (abs(ut*alpha1) .gT. 1.0 .or. abs(vt*alpha3) .gt. 1.0)then
!               IF (i.eq.572.and.j.eq.387)then
!                 print*,'i,j,it,ut,vt,div=', i,j,it,ut*alpha1,vt*alpha3,div(i,j)
!                 print*,'before i,j,uip1,uim1,vjp1,vjm1=', i,j,uip1,uim1,vjp1,vjm1
!               endif

                if(validpt(ip1,j))uip1=uip1+alpha1*ut
                if(validpt(im1,j))uim1=uim1-alpha2*ut
                if(validpt(i,jp1))vjp1=vjp1+alpha3*vt
                if(validpt(i,jm1))vjm1=vjm1-alpha4*vt

!               IF (i.eq.572.and.j.eq.387)then
!                 print*,'after i,j,uip1,uim1,vjp1,vjm1=', i,j,uip1,uim1,vjp1,vjm1
!               endif

                if(validpt(im1,j))u(im1,j)=uim1
                if(validpt(ip1,j))u(ip1,j)=uip1
                if(validpt(i,jm1))v(i,jm1)=vjm1
                if(validpt(i,jp1))v(i,jp1)=vjp1

!               IF (abs(ut*alpha1) .gT. 1.0)print*,'i,j,it,ut*alpha1=',i,j,it,ut*alpha1
!               IF (abs(vt*alpha3) .gT. 1.0)print*,'i,j,it,vt*alpha3=',i,j,it,vt*alpha3

                ijc=ijc+1
              endif
            else
              ij=ij+1
            endif
          enddo
          enddo

          print *,'iteration=', it, 'total number of pts calculated', ijc
          print *,'iteration=', it, 'total number of pts not calculated', ij

          divmax=-1.0e+09
          call divcel(validpt,u,v,div,nx,ny,dxm,dym,divmax)
30      continue

! Convergence Test for Divergence magnitude

      if(divmax <= divlim) then
        print*,'exiting loop because divergence is small, divmax,divlim=',divmax,divlim
        exit
      endif

20    continue
 

      where(validpt)
        diffu=U-usave
        diffv=V-vsave
      endwhere

      iu=0; iv=0
      do j=1,ny
      do i=1,nx
      if (abs(diffu(i,j)).gt.10. ) then
        print *, i,j,'DIFFU,DIFFV', diffu(i,j),diffv(i,j),'U ',usave(i,j), U(I,J),'MDL Topo',zsfc(i,j),'NDFD Topo',HTOPO(i,j)
!         print*,'usave,vsave=',usave(i+1,j),usave(i-1,j),vsave(i,j+1),vsave(i,j-1)
!         print*,'usave,unew,vsave,vnew=',usave(i,j),u(i,j),vsave(i,j),v(i,j)
        wndold = 1.944*sqrt(usave(i,j)*usave(i,j) + vsave(i,j)*vsave(i,j))
        wndnew = 1.944*sqrt(u(i,j)*u(i,j) + v(i,j)*v(i,j))
        print*,'wndold,wndnew=',wndold,wndnew
        if(wndnew > wndold)then
          print*, 'NEW WIND is STRONGER'
        endif
        iu=iu+1
      endif
      if (abs(diffv(i,j)).gt.10.)  then
        print *, i,j,'DIFFU,DIFFV', diffu(i,j),diffv(i,j),'V ',vsave(i,j), V(I,J),'MDL Topo',zsfc(i,j),'NDFD Topo',HTOPO(i,j)
!         print*,'usave,vsave=',usave(i+1,j),usave(i-1,j),vsave(i,j+1),vsave(i,j-1)
!         print*,'usave,unew,vsave,vnew=',usave(i,j),u(i,j),vsave(i,j),v(i,j)
        wndold = 1.944*sqrt(usave(i,j)*usave(i,j) + vsave(i,j)*vsave(i,j))
        wndnew = 1.944*sqrt(u(i,j)*u(i,j) + v(i,j)*v(i,j))
        if(wndnew > wndold)then
          print*, 'NEW WIND is STRONGER'
        endif
        iv=iv+1
      endif
      enddo
      enddo
      
      print *,'total number (diffu) > 10', iu
      print *,'total number (diffv) > 10', iv

      iu=0; iv=0
      do j=1,ny
      do i=1,nx
      if (abs(diffu(i,j)).gt.5. ) then
        print *, i,j,'DIFFU,DIFFV', diffu(i,j),diffv(i,j),'U ',usave(i,j), U(I,J),'MDL Topo',zsfc(i,j),'NDFD Topo',HTOPO(i,j)
!         print*,'usave,vsave=',usave(i+1,j),usave(i-1,j),vsave(i,j+1),vsave(i,j-1)
!         print*,'usave,unew,vsave,vnew=',usave(i,j),u(i,j),vsave(i,j),v(i,j)
        wndold = 1.944*sqrt(usave(i,j)*usave(i,j) + vsave(i,j)*vsave(i,j))
        wndnew = 1.944*sqrt(u(i,j)*u(i,j) + v(i,j)*v(i,j))
        print*,'wndold,wndnew=',wndold,wndnew
        if(wndnew > wndold)then
          print*, 'NEW WIND is STRONGER'
        endif
        iu=iu+1
      endif
      if (abs(diffv(i,j)).gt.5.)  then
        print *, i,j,'DIFFU,DIFFV', diffu(i,j),diffv(i,j),'V ',vsave(i,j), V(I,J),'MDL Topo',zsfc(i,j),'NDFD Topo',HTOPO(i,j)
!         print*,'usave,vsave=',usave(i+1,j),usave(i-1,j),vsave(i,j+1),vsave(i,j-1)
!         print*,'usave,unew,vsave,vnew=',usave(i,j),u(i,j),vsave(i,j),v(i,j)
        wndold = 1.944*sqrt(usave(i,j)*usave(i,j) + vsave(i,j)*vsave(i,j))
        wndnew = 1.944*sqrt(u(i,j)*u(i,j) + v(i,j)*v(i,j))
        print*,'wndold,wndnew=',wndold,wndnew
        if(wndnew > wndold)then
          print*, 'NEW WIND is STRONGER'
        endif
        iv=iv+1
      endif
      enddo
      enddo
      
      print *,'total number (diffu) > 5', iu
      print *,'total number (diffv) > 5', iv

      print *,'DIVMIN UWND  :',  MINVAL(U(:,:)),MAXVAL(U(:,:))
      print *,'DIVMIN VWND  :',  MINVAL(V(:,:)),MAXVAL(V(:,:))
      print *,'DIVMIN DIFF (new-old) UWND  :',  MINVAL(diffu(:,:)),MAXVAL(diffu(:,:))
      print *,'DIVMIN DIFF (new-old) VWND  :',  MINVAL(diffv(:,:)),MAXVAL(diffv(:,:))

      RETURN
      END

!----------------------------------------------------------------------
      SUBROUTINE divcel(validpt,u,v,div,nx,ny,dxm,dym,divmax)
!----------------------------------------------------------------------

! --- From CALMET Version 5.8 Level: 940304
! --- Code received from Jeff McQueen
! --- Code adapted from Subroutines DIVCEL(Annette Gibbs, February 2015)

!     INPUTS:  U (R ARRAY)     - GRIDDED X-DIRECTION WIND COMPONENTS
!              V (R ARRAY)     - GRIDDED Y-DIRECTION WIND COMPONENTS
!     OUTPUT:  DIV (R ARRAY)   - GRIDDED HORIZONTAL DIVERGENCE

      LOGICAL, INTENT(IN) :: VALIDPT(:,:)
      REAL, INTENT(INOUT) :: U(:,:),V(:,:)
      REAL, INTENT(INOUT) :: DIV(:,:)
      REAL, INTENT(IN) :: DXM,DYM
      INTEGER, INTENT(IN) :: NX,NY
      REAL dxi,dyi

! DX/DY are in meters.
      
      dxi=1.0/(2.0*dxm)
      dyi=1.0/(2.0*dym)

      print*,'dxm,dym,nx,ny,dxi,dyi=',dxm,dym,nx,ny,dxi,dyi

! Compute divergence (div=dudx+dvdy) using center in space differences

      do j=2,ny
      do i=2,nx
        div(i,j)=0.0
        ip1=i
        im1=i
        jp1=j
        jm1=j
        if(i.lt.nx)ip1=i+1
        if(i.gt.1)im1=i-1
        if(j.lt.ny)jp1=j+1
        if(j.gt.1)jm1=j-1
        if(validpt(i,j))then
          uip1=u(i,j)
          uim1=u(i,j)
          vjp1=v(i,j)
          vjm1=v(i,j)
          if(validpt(ip1,j))uip1=u(ip1,j)
          if(validpt(im1,j))uim1=u(im1,j)
          if(validpt(i,jp1))vjp1=v(i,jp1)
          if(validpt(i,jm1))vjm1=v(i,jm1)
          dudx=dxi*(uip1-uim1)
          dvdy=dyi*(vjp1-vjm1)
          div(i,j)=dudx+dvdy
          if(abs(div(i,j)) > 1.e3)then
           print*,'div,u,v=',i,j,div(i,j),uip1,uim1,vjp1,vjm1
           print*,'u,dudx,dxi*(uip1-uim1)=',i,j,u(ip1,j),u(im1,j),dxi*(uip1-uim1)
           print*,'v,dvdy,dyi*(vjp1-vjm1)=',i,j,v(i,jp1),v(i,jm1),dyi*(vjp1-vjm1)
           stop
          endif
          divabs=abs(div(i,j))
          divmax=amax1(divabs,divmax)
        endif
      enddo
      enddo

      print *,'DIVCEL DIVERGENCE  :',  MINVAL(DIV(:,:)),MAXVAL(DIV(:,:))

      RETURN
      END   
