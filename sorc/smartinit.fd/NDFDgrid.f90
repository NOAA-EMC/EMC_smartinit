      subroutine NDFDgrid(veg_nam_ndfd,tnew,dewnew,unew,vnew, &
      qnew,pnew,topo_ndfd,veg_ndfd,gdin,VALIDPT)
    use constants
    use grddef
    use aset2d
    use aset3d 
    use rdgrib     ! GRID and MASK defined in rdgrib
    
    REAL, INTENT(INOUT) :: TNEW(:,:),DEWNEW(:,:),UNEW(:,:),VNEW(:,:),PNEW(:,:)
    REAL, INTENT(INOUT) :: QNEW(:,:)
    REAL, INTENT(INOUT) :: VEG_NAM_NDFD(:,:),TOPO_NDFD(:,:),VEG_NDFD(:,:)
    LOGICAL, INTENT(INOUT) :: VALIDPT(:,:)
    TYPE (GINFO)        :: GDIN

    REAL, ALLOCATABLE   :: EXN(:,:) 
    REAL, ALLOCATABLE   :: ROUGH_MOD(:,:)
    REAL, ALLOCATABLE   :: TTMP(:,:),DTMP(:,:),UTMP(:,:),VTMP(:,:)

!    LOGICAL*1,   ALLOCATABLE   :: MASK(:)
!    REAL,        ALLOCATABLE   :: GRID(:)
    CHARACTER *4 CORE,REGION
    INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)

      real exn0,exn1, wsp
      integer nmod(2)
      integer i,j, ierr,k,ib,jb, ivar,ix,iy
      integer ibuf, ia,ja,iw,jw,id,n_rough_yes,n_rough_no
      integer m_rough_yes,m_rough_no
      real zs,qv,qq,e,enl,dwpt,z6,t6,gam,tsfc,td
      real tddep,td_orig,zdif_max,tup, qvdif2m5m,qv2m
      real qc,qvc,thetavc,uc,vc,ratio,speed,speedc,frac
      real tmean,dz,theta1,theta6,dx,dy
      logical ladjland,lconus,lnest,lhiresw,lvegtype
      character cvadj*1

 INTERFACE
    SUBROUTINE vadjust(VALIDPT,VEG_NDFD,U,V,HTOPO,DX,DY,IM,JM,LM,gdin)
    use constants
    use grddef
    use aset2d
    use aset3d

    LOGICAL, INTENT(IN) :: VALIDPT(:,:)
    REAL, INTENT(IN) :: VEG_NDFD(:,:)
    REAL, INTENT(INOUT) :: U(:,:),V(:,:)
    REAL, INTENT(IN) :: HTOPO(:,:),DX,DY
    TYPE (GINFO)        :: GDIN
    REAL, ALLOCATABLE   :: PHI(:,:,:)
    real HBAR,DXI,DYI,FX,FY,HTOIM1,HTOJM1,HTOIP1,HTOJP1,DHDX,DHDY, &
         DXSQ,DYSQ,DSQ,FACT,ERROR,ERR,EPSI,OVREL,XX,YY,XOLD,DSCALE
    integer itmax,ii,jj,kk,idir,it

    INTERFACE
    SUBROUTINE setphibnd(validpt,nx,ny,phi)
!==========================================================
!     Set PHI at validpt boundaries
!==========================================================
      LOGICAL, INTENT(IN) :: VALIDPT(:,:)
      REAL, INTENT(INOUT) :: PHI(:,:,:)
      INTEGER, INTENT(IN) :: NX,NY
     END SUBROUTINE setphibnd
    END INTERFACE

    END SUBROUTINE vadjust
 END INTERFACE

      print *, '***********************************'
      print *, 'Into NDFDgrid'
      print *, '***********************************'

      ispdsfc=1     ! Turn off/on friction adjustment for terrain
      call get_environment_variable("IVADJ",cvadj)  !turn on/off diagnostic wind adjustment
      print *, 'CVADJ for diagnostic wind adjust: ',CVADJ, '   friction adj: ',ispdsfc

      IM=gdin%IMAX;JM=gdin%JMAX;LM=gdin%KMAX
      iprt=int(im/2);jprt=int(jm/2);kprt=0;lprt=0;kkprt=0
      print*,'iprt,jprt=',iprt,jprt
      ITOT=IM*JM
      core=gdin%core  ! For hiresw runs, nmmb core treated differently
      lhiresw=gdin%lhiresw  ! For hiresw runs, nmmb core treated differently
      region=gdin%region
      ladjland=.false.
      lconus=.false.
      lvegtype=.false.
      lnest=gdin%lnest

! specific grid points to print for debugging
      if (region .eq. 'CS2P')then
        iprt1=649; jprt1=694 ! highest elevation on model grid
        iprt2=687; jprt2=750 ! highest elevation on topo ndfd grid
! topo .le. model terrain (zs)
!       iprt3=1583;jprt3=52 ! k=1 and zs > topo
        iprt3=2058;jprt3=1126 ! k=1 and zs > topo
! iprt and jprt ! topo = zs
!       iprt4=2004; jprt4=35 ! k=1 and zs < topo
        iprt4=100; jprt4=1000 ! k=1 and zs < topo
        iprt5=683; jprt5=1 ! zs and topo differ by more than 100 m
        iprt6=307; jprt6=652 ! topo is negative (-77)
      endif 

! For reanalysis, lnest = True

      print*,'core, region,lnest=', core, region, lnest

      ALLOCATE (EXN(IM,JM),ROUGH_MOD(IM,JM),STAT=kret)
      ALLOCATE (TTMP(IM,JM),DTMP(IM,JM),STAT=kret)
      ALLOCATE (UTMP(IM,JM),VTMP(IM,JM),STAT=kret)

!  read in 5 km topography
!  changed name for consistency with non-conus region names
!  changed to unit 48 for consistency with other domains
!  CHANGE to read GRIB FILES for non-conus regions 

      DX=5000.;DY=5000.  ! HARD WIRED for 5 km output grids (Conus,hi,pr)
!Check if hi/pr grids are reduced to 2.5 km
      if (region .eq. 'CS' ) then
        lconus=.TRUE.;lvegtype=.true.
        print *, 'read in Binary topo and veg files '
        open (46, file='TOPONDFD', form='unformatted')
        read (46) topo_ndfd
        close (46)
     
!       Read in 5 km vegetation for CONUS domain
        open (48, file='LANDNDFD', form='unformatted')
        read (48) veg_ndfd
        close (48)

      else 
        rghlim=0.5
        veglim=0.5
        scale=100.

!      All NDFD grids including Extended CONUS csp2 grid, water=0
       ivgid=81 
!      FOR VEG_NDFD CS2P grid 184, input is Veg type:  water=16
       if (region .eq. 'CS2P') ivgid=225 

        print*, ' gdin%region: ', gdin%region
        print *, 'READ IN NDFD GRIB  TOPO file'
        JGDS=-1
        CALL RDHDRS(46,47,IGDNUM,GDIN,NUMVAL)
        print *, 'IGDNUM',IGDNUM,' NUMVAL',NUMVAL
        DEALLOCATE(GRID,MASK)
        ALLOCATE (GRID(NUMVAL),MASK(NUMVAL),STAT=kret)
        print *,'GRID, MASK Allocated  STAT=',STAT,NUMVAL
        J=-1;JPDS=-1;JGDS=-1
        JPDS(3)=IGDNUM;JPDS(5)=8;JPDS(6)=1
         
        CALL SETVAR(46,47,NUMVAL,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,topo_ndfd,IRET,ISTAT)
!        DX=JGDS(9)
!        DY=JGDS(10)

        DX=2500.;DY=2500. ! hardwired for conus nests
        if(region.eq.'AK3') then 
          DX=3000.
          DY=3000. 
        elseif(region.eq.'AK') then 
          DX=6000.
          DY=6000. 
        endif

        print *,REGION,'  DX DY ',DX,DY,im,jm,NUMVAL

        print *, 'READ IN NDFD GRIB LAND COVER file'
        CALL RDHDRS(48,49,IGDNUM,GDIN,NUMVAL)
        J=0;JPDS=-1;JPDS(3)=IGDNUM;JPDS(5)=ivgid;JPDS(6)=1;JPDS(7)=0;JGDS=-1
        CALL SETVAR(48,49,NUMVAL,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,veg_ndfd,IRET,ISTAT)

        if (LHIRESW .or. REGION .eq. 'CS2P') then 
          if (region.eq.'CS2P') lconus=.TRUE.
          lvegtype=.true.     ! or = false, then use veg fraction
          where (veg_ndfd.le.0.) veg_ndfd=16. 
        endif
        DEALLOCATE(GRID,MASK)
      endif
     
      if(lvegtype) then 
        rghlim=0.05  ! Make consistent with ndfd land fraction 
        veglim=16.
        scale=1.
        print *, 'NDFD Land Mask grid file is VEG Fraction',rghlim, veglim 
        print *, 'Convert Model land use to VEG Fraction (0.0 or 0.1)'
        where(veg_nam_ndfd.eq.16.) veg_nam_ndfd = -1.
        where(veg_nam_ndfd.ne.16. .and. veg_nam_ndfd.gt.0.) veg_nam_ndfd = 0.10
        where(veg_nam_ndfd.eq.-1.) veg_nam_ndfd =  0.
      endif

      print *,'NDFD TOPO: ',MINVAL(topo_ndfd),MAXVAL(topo_ndfd)
      print *,'MDL  TOPO: ',MINVAL(zsfc),MAXVAL(zsfc)
      print* , 'veglim,rghlim: ', veglim,rghlim, ' grib vgid: ', ivgid
      print *,'NDFD MASK: ',MINVAL(veg_ndfd),MAXVAL(veg_ndfd)
      print *,'MDL  MASK: ',MINVAL(veg_nam_ndfd),MAXVAL(veg_nam_ndfd)
      print *,'MDL Q :       ',MINVAL(q),MAXVAL(q)
      print *,'MDL Q2:       ',MINVAL(q2),MAXVAL(q2)
      print *,'MDL  T:       ',MINVAL(T),MAXVAL(T)
      print *,'MDL GEOP HGT: ',MINVAL(hght),MAXVAL(hght)
      print *,'lconus,lnest,lvegtype ',lconus,lnest,lvegtype

      zdif_max = -1000.
      n_rough_yes=0
      n_rough_no =0
!C ****************************************************************
! -- Now let's start reducing to NDFD topo elevation.
!C ****************************************************************
      zmax=0
      tmax=0
! Find highest point on the grid [AMG]
      do j=1,jm
      do i=1,im
! model
      if(zsfc(i,j) .lt. 0.)print*, 'i,j,zsfc=',i,j,zsfc(i,j)
      if(zsfc(i,j).gt.zmax)then
      zmax=zsfc(i,j)
      isav=i
      jsav=j
      endif
! NDFD topo
      if(topo_ndfd(i,j) .lt. 0.)then
         print*, 'i,j,topo_ndfd,zsfc=',i,j,topo_ndfd(i,j),zsfc(i,j)
      endif
      if(topo_ndfd(i,j).gt.tmax)then
      tmax=topo_ndfd(i,j)
      itsav=i
      jtsav=j
      endif
      enddo
      enddo
      print*,'zmax,isav,jsav=',zmax,isav,jsav
      print*,'topo at zmax=',topo_ndfd(isav,jsav)
      print*,'tmax,itsav,jtsav=',tmax,itsav,jtsav
      print*,'model terrain at tmax=',zsfc(itsav,jtsav)
      where (zsfc .lt. 0.) zsfc=0.0
      tnew=spval;qnew=spval
      dewnew=spval;unew=spval;vnew=spval
      pnew=spval  

      do 120 j=1,jm
      do 120 i=1,im
        if (.not. validpt(i,j)) goto 120
! This does not appear to be used [AMG]
        exn(i,j) = cpd_p*(psfc(i,j)/P1000)**rovcp_p
! ---   z = surface elevation
        zs = zsfc(i,j)

! --- q = specific humidity at 2m from NAM model sfc
! --- dew-point temperature at original sfc
        td_orig=d2(i,j)

! --- dewpoint depression
! --- at original sfc [AMG]
        tddep = max(0.,t2(i,j) - td_orig )
! I don't think this is actually used at this point; but still setting to q2
! because q(i,j,1) is q at 1000 mb and could be underground at some gridpoints [AMG]
!       qv= q(i,j,1)
        qv= q2(i,j)
        QQ = QV/(1.+QV)
! Need to reset as tp1 could be underground - tp1 is temp at 1000 mb. [AMG]
!       tp1=T(I,J,1)
! T30 is temperature at constant BL pressure layer of 0-30 mb (about 15 mb above ground) [AMG]
        tp1=T30(I,J)
          
! --- Base Td on 2m q
        qv = qq/(1.-qq)

! ---   get values at level 6 for lapse rate calculations - NAM Nest
! ---   get values at level 3 for lapse rate calculations - NARR (pressure level data)
!       QQ = Q(I,J,6)/(1.+Q(i,j,6))
!       QQ = Q30(I,J)/(1.+Q30(i,j))

!       exn(i,j) = cpd_p*(pmid(i,j,6)/P1000)**rovcp_p
!       T6=T(I,J,6)
!       Z1=HGHT(I,J,1)
!       Z6=HGHT(I,J,6)
!       GAM = (TP1-T6)/(Z6-Z1)
! Subtract 165 mb from psfc to get midpoint of boundary layer between 180-150 mb [AMG]
! Again, I don't think exn is actually used [AMG]
        p165=psfc(i,j)-16500.
        exn(i,j) = cpd_p*(p165/P1000)**rovcp_p
        T6=T180(I,J)
! No height level for boundary layer; calculate height at boundary layer  pressures
        p15=psfc(i,j)-1500. ! 15 mb is between 30 and 0 mb above ground
        do k=1,lm
          if(p165 .gt. pmid(i,j,k))goto 165
        enddo
165       if(k.eq.1)then
            print*,'k=1 at p165,i,j=',k,i,j
            tmn = (t6 + t(i,j,k)) * 0.5 
            h165 = hght(i,j,k) + (RD_P*tmn/G0_P) * log(pmid(i,j,k)/p165)
!         print*,'i,j,k,t6,p165,tmn,t,pmid,hght=',i,j,k,t6,p165,tmn,t(i,j,k-1),pmid(i,j,k-1),hght(i,j,k-1)
          print*,'k=1 @ psfc-165 mb'
          print*,'i,j,k,h165,psfc,t,pmid,hght=',i,j,k,h165,psfc(i,j),t(i,j,k),pmid(i,j,k),hght(i,j,k)
          else
            tmn = (t6 + t(i,j,k-1)) * 0.5 
            h165 = hght(i,j,k-1) + (RD_P*tmn/G0_P) * log(pmid(i,j,k-1)/p165)
        if(i.eq.iprt1 .and. j .eq.jprt1)then
          print*,'highest elevation on model grid'
          print*,'k,t6,p165,tmn,t,pmid,hght=',k,t6,p165,tmn,t(i,j,k-1),pmid(i,j,k-1),hght(i,j,k-1)
          print*,'h165,psfc,t,pmid,hght=',h165,psfc(i,j),t(i,j,k),pmid(i,j,k),hght(i,j,k)
        endif
        if(i.eq.iprt2 .and.  j.eq.jprt2)then
          print*,'highest elevation on topo grid'
          print*,'k,t6,p165,tmn,t,pmid,hght=',k,t6,p165,tmn,t(i,j,k-1),pmid(i,j,k-1),hght(i,j,k-1)
          print*,'h165,psfc,t,pmid,hght=',h165,psfc(i,j),t(i,j,k),pmid(i,j,k),hght(i,j,k)
        endif
        if(i.eq.iprt3 .and.  j.eq.jprt3)then
          print*,'k=1 and topo < zs'
          print*,'k,t6,p165,tmn,t,pmid,hght=',k,t6,p165,tmn,t(i,j,k-1),pmid(i,j,k-1),hght(i,j,k-1)
          print*,'h165,psfc,t,pmid,hght=',h165,psfc(i,j),t(i,j,k),pmid(i,j,k),hght(i,j,k)
        endif
        if(i.eq.iprt4 .and.  j.eq.jprt4)then
          print*,'k=1 and topo > zs'
          print*,'k,t6,p165,tmn,t,pmid,hght=',k,t6,p165,tmn,t(i,j,k-1),pmid(i,j,k-1),hght(i,j,k-1)
          print*,'h165,psfc,t,pmid,hght=',h165,psfc(i,j),t(i,j,k),pmid(i,j,k),hght(i,j,k)
        endif
        if(i.eq.iprt5 .and.  j.eq.jprt5)then
          print*,'zs and topo differ by more than 100 m'
          print*,'k,t6,p165,tmn,t,pmid,hght=',k,t6,p165,tmn,t(i,j,k-1),pmid(i,j,k-1),hght(i,j,k-1)
          print*,'h165,psfc,t,pmid,hght=',h165,psfc(i,j),t(i,j,k),pmid(i,j,k),hght(i,j,k)
        endif
        if(i.eq.iprt6 .and.  j.eq.jprt6)then
          print*,'topo is negative (-77)'
          print*,'k,t6,p165,tmn,t,pmid,hght=',k,t6,p165,tmn,t(i,j,k-1),pmid(i,j,k-1),hght(i,j,k-1)
          print*,'h165,psfc,t,pmid,hght=',h165,psfc(i,j),t(i,j,k),pmid(i,j,k),hght(i,j,k)
        endif
          endif
        do k=1,lm
          if(p15 .gt. pmid(i,j,k))goto 166
        enddo
166       if(k.eq.1)then
            tmn = (tp1 + t(i,j,k)) * 0.5 
            h15 = hght(i,j,k) + (RD_P*tmn/G0_P) * log(pmid(i,j,k)/p15)
!       if(i.eq.2133 .and.  j.eq.687)then
!       if(zs .gt. topo_ndfd(i,j))then
!       if((zs-topo_ndfd(i,j)) .gt. 100.)then
!         print*,'greater i,j,k,tp1,p15,tmn,t,pmid,hght=',i,j,k,tp1,p15,tmn,t(i,j,k),pmid(i,j,k),hght(i,j,k)
!         print*,'greater i,j,k,h15,psfc,t,pmid,hght=',i,j,k,h15,psfc(i,j),t(i,j,k),pmid(i,j,k),hght(i,j,k)
!       endif
!       if(zs .lt. topo_ndfd(i,j))then
!       if((topo_ndfd(i,j)-zs) .gt. 100.)then
!         print*,'less i,j,k,tp1,p15,tmn,t,pmid,hght=',i,j,k,tp1,p15,tmn,t(i,j,k),pmid(i,j,k),hght(i,j,k)
!         print*,'less i,j,k,h15,psfc,t,pmid,hght=',i,j,k,h15,psfc(i,j),t(i,j,k),pmid(i,j,k),hght(i,j,k)
!       endif
        if(i.eq.iprt1 .and. j .eq.jprt1)then
          print*,'highest elevation on model grid'
!         print*,'k,tp1,p15,tmn,t,pmid,hght=',k,tp1,p15,tmn,t(i,j,k-1),pmid(i,j,k-1),hght(i,j,k-1)
          print*,'k,tp1,p15,tmn,h15,psfc,t,pmid,hght=',k,tp1,p15,tmn,h15,psfc(i,j),t(i,j,k),pmid(i,j,k),hght(i,j,k)
        endif
        if(i.eq.iprt2 .and.  j.eq.jprt2)then
          print*,'highest elevation on topo grid'
!         print*,'k,tp1,p15,tmn,t,pmid,hght=',k,tp1,p15,tmn,t(i,j,k-1),pmid(i,j,k-1),hght(i,j,k-1)
          print*,'k,tp1,p15,tmn,h15,psfc,t,pmid,hght=',k,tp1,p15,tmn,h15,psfc(i,j),t(i,j,k),pmid(i,j,k),hght(i,j,k)
        endif
        if(i.eq.iprt3 .and.  j.eq.jprt3)then
          print*,'k=1 and topo < zs'
!         print*,'k,tp1,p15,tmn,t,pmid,hght=',k,tp1,p15,tmn,t(i,j,k-1),pmid(i,j,k-1),hght(i,j,k-1)
          print*,'k,tp1,p15,tmn,h15,psfc,t,pmid,hght=',k,tp1,p15,tmn,h15,psfc(i,j),t(i,j,k),pmid(i,j,k),hght(i,j,k)
        endif
        if(i.eq.iprt4 .and.  j.eq.jprt4)then
          print*,'k=1 and topo > zs'
!         print*,'k,tp1,p15,tmn,t,pmid,hght=',k,tp1,p15,tmn,t(i,j,k-1),pmid(i,j,k-1),hght(i,j,k-1)
          print*,'k,tp1,p15,tmn,h15,psfc,t,pmid,hght=',k,tp1,p15,tmn,h15,psfc(i,j),t(i,j,k),pmid(i,j,k),hght(i,j,k)
        endif
        if(i.eq.iprt5 .and.  j.eq.jprt5)then
          print*,'zs and topo differ by more than 100 m'
          !rint*,'k,tp1,p15,tmn,t,pmid,hght=',k,tp1,p15,tmn,t(i,j,k-1),pmid(i,j,k-1),hght(i,j,k-1)
          print*,'k,tp1,p15,tmn,h15,psfc,t,pmid,hght=',k,tp1,p15,tmn,h15,psfc(i,j),t(i,j,k),pmid(i,j,k),hght(i,j,k)
        endif
        if(i.eq.iprt6 .and.  j.eq.jprt6)then
          print*,'topo is negative (-77)'
!         print*,'k,tp1,p15,tmn,t,pmid,hght=',k,tp1,p15,tmn,t(i,j,k-1),pmid(i,j,k-1),hght(i,j,k-1)
          print*,'k,tp1,p15,tmn,h15,psfc,t,pmid,hght=',k,tp1,p15,tmn,h15,psfc(i,j),t(i,j,k),pmid(i,j,k),hght(i,j,k)
        endif
          else
            tmn = (tp1 + t(i,j,k-1)) * 0.5 
            h15 = hght(i,j,k-1) + (RD_P*tmn/G0_P) * log(pmid(i,j,k-1)/p15)
        if(i.eq.iprt1 .and. j.eq.jprt1)then
          print*,'highest elevation on model grid'
          print*,'k,tp1,p15,tmn,t,pmid,hght=',k,tp1,p15,tmn,t(i,j,k-1),pmid(i,j,k-1),hght(i,j,k-1)
          print*,'h15,psfc,t,pmid,hght=',h15,psfc(i,j),t(i,j,k),pmid(i,j,k),hght(i,j,k)
        endif
        if(i.eq.iprt2 .and.  j.eq.jprt2)then
          print*,'highest elevation on topo grid'
          print*,'k,tp1,p15,tmn,t,pmid,hght=',k,tp1,p15,tmn,t(i,j,k-1),pmid(i,j,k-1),hght(i,j,k-1)
          print*,'h15,psfc,t,pmid,hght=',h15,psfc(i,j),t(i,j,k),pmid(i,j,k),hght(i,j,k)
        endif
        if(i.eq.iprt3 .and.  j.eq.jprt3)then
          print*,'k=1 and topo < zs'
          print*,'k,tp1,p15,tmn,t,pmid,hght=',k,tp1,p15,tmn,t(i,j,k-1),pmid(i,j,k-1),hght(i,j,k-1)
          print*,'h15,psfc,t,pmid,hght=',h15,psfc(i,j),t(i,j,k),pmid(i,j,k),hght(i,j,k)
        endif
        if(i.eq.iprt4 .and.  j.eq.jprt4)then
          print*,'k=1 and topo > zs'
          print*,'k,tp1,p15,tmn,t,pmid,hght=',k,tp1,p15,tmn,t(i,j,k-1),pmid(i,j,k-1),hght(i,j,k-1)
          print*,'h15,psfc,t,pmid,hght=',h15,psfc(i,j),t(i,j,k),pmid(i,j,k),hght(i,j,k)
        endif
        if(i.eq.iprt5 .and.  j.eq.jprt5)then
          print*,'zs and topo differ by more than 100 m'
          print*,'k,tp1,p15,tmn,t,pmid,hght=',k,tp1,p15,tmn,t(i,j,k-1),pmid(i,j,k-1),hght(i,j,k-1)
          print*,'h15,psfc,t,pmid,hght=',h15,psfc(i,j),t(i,j,k),pmid(i,j,k),hght(i,j,k)
        endif
        if(i.eq.iprt6 .and.  j.eq.jprt6)then
          print*,'topo is negative (-77)'
          print*,'k,tp1,p15,tmn,t,pmid,hght=',k,tp1,p15,tmn,t(i,j,k-1),pmid(i,j,k-1),hght(i,j,k-1)
          print*,'h15,psfc,t,pmid,hght=',h15,psfc(i,j),t(i,j,k),pmid(i,j,k),hght(i,j,k)
        endif
          endif
        GAM = (TP1-T6)/(-(p165-p15))
        if(i.eq.iprt1 .and.  j.eq.jprt1)print*,'dtdp i,j,gam,tp1,t6,p165,p15=',i,j,gam,tp1,t6,p165,p15
        if(i.eq.iprt2 .and.  j.eq.jprt2)print*,'dtdp i,j,gam,tp1,t6,p165,p15=',i,j,gam,tp1,t6,p165,p15
        if(i.eq.iprt3 .and.  j.eq.jprt3)print*,'dtdp i,j,gam,tp1,t6,p165,p15=',i,j,gam,tp1,t6,p165,p15
        if(i.eq.iprt4 .and.  j.eq.jprt4)print*,'dtdp i,j,gam,tp1,t6,p165,p15=',i,j,gam,tp1,t6,p165,p15
        if(i.eq.iprt5 .and.  j.eq.jprt5)print*,'dtdp i,j,gam,tp1,t6,p165,p15=',i,j,gam,tp1,t6,p165,p15
        if(i.eq.iprt6 .and.  j.eq.jprt6)print*,'dtdp i,j,gam,tp1,t6,p165,p15=',i,j,gam,tp1,t6,p165,p15
        GAM = (TP1-T6)/(h165-h15)
        if(i.eq.iprt1 .and.  j.eq.jprt1)print*,'dtdz i,j,gam,tp1,t6,p165,p15=',i,j,gam,tp1,t6,h165,h15
        if(i.eq.iprt2 .and.  j.eq.jprt2)print*,'dtdz i,j,gam,tp1,t6,p165,p15=',i,j,gam,tp1,t6,h165,h15
        if(i.eq.iprt3 .and.  j.eq.jprt3)print*,'dtdz i,j,gam,tp1,t6,p165,p15=',i,j,gam,tp1,t6,h165,h15
        if(i.eq.iprt4 .and.  j.eq.jprt4)print*,'dtdz i,j,gam,tp1,t6,p165,p15=',i,j,gam,tp1,t6,h165,h15
        if(i.eq.iprt5 .and.  j.eq.jprt5)print*,'dtdz i,j,gam,tp1,t6,p165,p15=',i,j,gam,tp1,t6,h165,h15
        if(i.eq.iprt6 .and.  j.eq.jprt6)print*,'dtdz i,j,gam,tp1,t6,p165,p15=',i,j,gam,tp1,t6,h165,h15

!============================================
        if (topo_ndfd(i,j).le.zs ) then
!============================================
          GAM = MIN(GAMD,MAX(GAM,GAMi))

! --- temperature at NDFD topo
! -- again, use 2m T at NAM regular terrain from similarity
!      theory for derivation of 2m T at topomini elevation
          tsfc = t2(i,j) + (zs-topo_ndfd(i,j))*gam
        if(i.eq.iprt1 .and.  j.eq.jprt1)print*,'3i,j,gam,gamd,gami,tsfc,t2,td_orig=',i,j,gam,gamd,gami,tsfc,t2(i,j),td_orig
        if(i.eq.iprt2 .and.  j.eq.jprt2)print*,'3i,j,gam,gamd,gami,tsfc,t2,td_orig=',i,j,gam,gamd,gami,tsfc,t2(i,j),td_orig
        if(i.eq.iprt3 .and.  j.eq.jprt3)print*,'3i,j,gam,gamd,gami,tsfc,t2,td_orig=',i,j,gam,gamd,gami,tsfc,t2(i,j),td_orig
        if(i.eq.iprt4 .and.  j.eq.jprt4)print*,'3i,j,gam,gamd,gami,tsfc,t2,td_orig=',i,j,gam,gamd,gami,tsfc,t2(i,j),td_orig
        if(i.eq.iprt5 .and.  j.eq.jprt5)print*,'3i,j,gam,gamd,gami,tsfc,t2,td_orig=',i,j,gam,gamd,gami,tsfc,t2(i,j),td_orig
        if(i.eq.iprt6 .and.  j.eq.jprt6)print*,'3i,j,gam,gamd,gami,tsfc,t2,td_orig=',i,j,gam,gamd,gami,tsfc,t2(i,j),td_orig

!  Don't let reduced valley temps be
!     any lower than NAM 2m temp minus 10K.
          tsfc = max(t2(i,j)-10.,tsfc)
!  Can't let valley temps go below NAM dewpoint temps.
          tsfc = max (tsfc,td_orig)

! --- pressure at NDFD topo
          tmean = (tsfc+t2(i,j)) * 0.5
          dz = zs-topo_ndfd(i,j)
          pnew(i,j) = psfc(i,j) * exp(g0_p*dz/(rd_p*tmean))

! --- temperature
          tnew(i,j) = tsfc
        if(i.eq.iprt1 .and.  j.eq.jprt1)print*,'NDFD < MDL topo ',i,j,validpt(i,j),tnew(i,j),t2(i,j),topo_ndfd(i,j),zs,psfc(i,j),pnew(i,j)
        if(i.eq.iprt2 .and.  j.eq.jprt2)print*,'NDFD < MDL topo ',i,j,validpt(i,j),tnew(i,j),t2(i,j),topo_ndfd(i,j),zs,psfc(i,j),pnew(i,j)
        if(i.eq.iprt3 .and.  j.eq.jprt3)print*,'NDFD < MDL topo ',i,j,validpt(i,j),tnew(i,j),t2(i,j),topo_ndfd(i,j),zs,psfc(i,j),pnew(i,j)
        if(i.eq.iprt4 .and.  j.eq.jprt4)print*,'NDFD < MDL topo ',i,j,validpt(i,j),tnew(i,j),t2(i,j),topo_ndfd(i,j),zs,psfc(i,j),pnew(i,j)
        if(i.eq.iprt5 .and.  j.eq.jprt5)print*,'NDFD < MDL topo ',i,j,validpt(i,j),tnew(i,j),t2(i,j),topo_ndfd(i,j),zs,psfc(i,j),pnew(i,j)
        if(i.eq.iprt6 .and.  j.eq.jprt6)print*,'NDFD < MDL topo ',i,j,validpt(i,j),tnew(i,j),t2(i,j),topo_ndfd(i,j),zs,psfc(i,j),pnew(i,j)
!         if (i.lt.iprt.and.j.lt.jprt.and.lprt.eq.0.and.topo_ndfd(i,j).lt.zs.and. (zs-topo_ndfd(i,j).gt.100.))then
!         if (lprt.eq.0.and.topo_ndfd(i,j).lt.zs.and. (zs-topo_ndfd(i,j).gt.100.))then
!           print *,'50NDFD < MDL topo ',i,j,validpt(i,j),tnew(i,j),t2(i,j),topo_ndfd(i,j),zs,psfc(i,j),pnew(i,j)
!           lprt=1
!         endif

! Set dewpoint depression to that at original sfc

! --- dew-pt at topomini
          dewnew(i,j) = tsfc - tddep

! --- surface winds
! -- use 10 m wind values derived from similarity theory
!   gsm  use u and v of level 1 or 10m???
          unew(i,j) = u10(i,j)
          vnew(i,j) = v10(i,j)

          if (i.eq.iprt1.and.j.eq.jprt1)print *,'NDFD < MDL topo ',i,j,dewnew(i,j),d2(i,j),topo_ndfd(i,j),zs,unew(i,j),vnew(i,j)
          if (i.eq.iprt2.and.j.eq.jprt2)print *,'NDFD < MDL topo ',i,j,dewnew(i,j),d2(i,j),topo_ndfd(i,j),zs,unew(i,j),vnew(i,j)
          if (i.eq.iprt3.and.j.eq.jprt3)print *,'NDFD < MDL topo ',i,j,dewnew(i,j),d2(i,j),topo_ndfd(i,j),zs,unew(i,j),vnew(i,j)
          if (i.eq.iprt4.and.j.eq.jprt4)print *,'NDFD < MDL topo ',i,j,dewnew(i,j),d2(i,j),topo_ndfd(i,j),zs,unew(i,j),vnew(i,j)
          if (i.eq.iprt5.and.j.eq.jprt5)print *,'NDFD < MDL topo ',i,j,dewnew(i,j),d2(i,j),topo_ndfd(i,j),zs,unew(i,j),vnew(i,j)
          if (i.eq.iprt6.and.j.eq.jprt6)print *,'NDFD < MDL topo ',i,j,dewnew(i,j),d2(i,j),topo_ndfd(i,j),zs,unew(i,j),vnew(i,j)
!============================================
        ELSE if (topo_ndfd(i,j).gt.zs) then
!============================================
! ----  Now only if topo_NDFD is above the model elevation

!        Here, when topo-NDFD > topo-MDL, we allow a small
!        subisothermal lapse rate with slight warming with height.

          GAM = MIN(GAMD,MAX(GAM,GAMsubj))

          DO K=1,LM
!          if (hght(i,j,k) .gt. topo_ndfd(i,j)) goto 781
!          if (hght(i,j,k) .gt. topo_ndfd(i,j) .and. hght(i,j,k-1) .lt. zs)then
!          if (hght(i,j,k) .gt. topo_ndfd(i,j) .and. hght(i,j,k-1) .ge. zs) goto 781
!          if (hght(i,j,k) .gt. topo_ndfd(i,j) .and. hght(i,j,k) .ge. zs .and. k.eq.1) goto 781
           if (hght(i,j,k) .gt. topo_ndfd(i,j) .and. hght(i,j,k) .ge. zs .and. pmid(i,j,k) .ne. psfc(i,j))then
           if (i.eq.iprt1 .and. j.eq.jprt1 ) then
             print*,'i,j,k,pmid,zs,topo,hght=',i,j,k,pmid(i,j,k),zs,topo_ndfd(i,j),hght(i,j,k)
             print*,'i,j,k,gam,gamd,gamsubj=',i,j,k,gam,gamd,gamsubj
           endif
           if (i.eq.iprt2 .and. j.eq.jprt2 ) then
             print*,'i,j,k,pmid,zs,topo,hght=',i,j,k,pmid(i,j,k),zs,topo_ndfd(i,j),hght(i,j,k)
             print*,'i,j,k,gam,gamd,gamsubj=',i,j,k,gam,gamd,gamsubj
           endif
           if (i.eq.iprt3 .and. j.eq.jprt3 ) then
             print*,'i,j,k,pmid,zs,topo,hght=',i,j,k,pmid(i,j,k),zs,topo_ndfd(i,j),hght(i,j,k)
             print*,'i,j,k,gam,gamd,gamsubj=',i,j,k,gam,gamd,gamsubj
           endif
           if (i.eq.iprt4 .and. j.eq.jprt4 ) then
             print*,'i,j,k,pmid,zs,topo,hght=',i,j,k,pmid(i,j,k),zs,topo_ndfd(i,j),hght(i,j,k)
             print*,'i,j,k,gam,gamd,gamsubj=',i,j,k,gam,gamd,gamsubj
           endif
           if (i.eq.iprt5 .and. j.eq.jprt5 ) then
             print*,'i,j,k,pmid,zs,topo,hght=',i,j,k,pmid(i,j,k),zs,topo_ndfd(i,j),hght(i,j,k)
             print*,'i,j,k,gam,gamd,gamsubj=',i,j,k,gam,gamd,gamsubj
           endif
           if (i.eq.iprt6 .and. j.eq.jprt6 ) then
             print*,'i,j,k,pmid,zs,topo,hght=',i,j,k,pmid(i,j,k),zs,topo_ndfd(i,j),hght(i,j,k)
             print*,'i,j,k,gam,gamd,gamsubj=',i,j,k,gam,gamd,gamsubj
           endif
             if(pmid(i,j,k) .eq.  psfc(i,j))print*,'i,j,k,pmid,psfc,hght,zs=',i,j,k,pmid(i,j,k),psfc(i,j),hght(i,j,k),zs
             goto 781
           endif
          ENDDO 
  781     continue

          if (k .eq. 1) then
!           print*,'k=1,i,j,zs,topo_ndfd(i,j)=',k,i,j,zs,topo_ndfd(i,j)
            zbot=zs
            pbot=psfc(i,j)
            tbot=t2(i,j)
            qbot=q2(i,j)
          else 
            zbot=hght(i,j,k-1)
            pbot=pmid(i,j,k-1)
            tbot=t(i,j,k-1)
            qbot=q(i,j,k-1)
          endif
          frac = (topo_ndfd(i,j)-zbot) / (hght(i,j,k)-zbot)
          exn1 = (pbot/P1000)**rovcp_p
          exn0 = (pmid(i,j,k)/P1000)**rovcp_p
! --- pressure at NDFD topo
          pnew(i,j) = P1000* ((exn1 +frac * (exn0 - exn1)) **cpovr_p)
          thetak=((P1000/PMID(i,j,k))**CAPA)*T(i,j,k)
          thetak1=((P1000/pbot)**CAPA)*tbot  
          thetavc = thetak1+frac * (thetak-thetak1)
          qvc = qbot+frac * (Q(i,j,k)-qbot)
          qc = qvc/(1.+qvc)

! --- temperature
! GSM changed the tup computation, as it appears to give
!   a better-looking product.  need to revisit at some point 
          tup=t2(i,j)+frac*(t(i,j,k)-t2(i,j))

! Is Tup already Temperature for nests ???????  
          if (.not.lconus) &
            tup = thetavc*(pnew(i,j)/P1000)**rovcp_p/(1.+0.6078*qc)
            
!  provisional 2m temp at NDFD topo
          tnew(i,j) = t2(i,j) + (tup-tp1)

! --- Dont let extrapolated temp to be any larger than
!     the value at the NAM terrain level.
!     This will avoid the problem with NDFD temp values
!     being set to be much warmer than NAM 2m temp.

          tsfc=t2(i,j) + (zs-topo_ndfd(i,j))*gam

          if (tnew(i,j) .gt. t2(i,j))  tnew(i,j) = min(tnew(i,j),tsfc)
           if (i.eq.iprt1 .and. j.eq.jprt1 ) then
           print *,'NDFD > MDL topo,tnew,topo,zs',i,j,tnew(i,j),topo_ndfd(i,j),zs
           print *,' pnew ',pnew(i,j),'psfc ', psfc(i,j),' thetavc ',thetavc, 't2', t2(i,j)
           print*,'pbot,zbot,tbot,qbot=',k,pbot,zbot,tbot,qbot
           print*,'frac,T,tup,tp1,tsfc,tnew=',frac,T(i,j,k),tup,tp1,tsfc,tnew(i,j)
          endif
           if (i.eq.iprt2 .and. j.eq.jprt2 ) then
           print *,'NDFD > MDL topo,tnew,topo,zs',i,j,tnew(i,j),topo_ndfd(i,j),zs
           print *,' pnew ',pnew(i,j),'psfc ', psfc(i,j),' thetavc ',thetavc, 't2', t2(i,j)
           print*,'pbot,zbot,tbot,qbot=',k,pbot,zbot,tbot,qbot
           print*,'frac,T,tup,tp1,tsfc,tnew=',frac,T(i,j,k),tup,tp1,tsfc,tnew(i,j)
          endif
           if (i.eq.iprt3 .and. j.eq.jprt3 ) then
           print *,'NDFD > MDL topo,validpt,tnew,topo,zs',validpt(i,j),tnew(i,j),topo_ndfd(i,j),zs
           print *,' pnew ',pnew(i,j),'psfc ', psfc(i,j),' thetavc ',thetavc, 't2', t2(i,j)
           print*,'pbot,zbot,tbot,qbot=',k,pbot,zbot,tbot,qbot
           print*,'frac,T,tup,tp1,tsfc,tnew=',frac,T(i,j,k),tup,tp1,tsfc,tnew(i,j)
          endif
           if (i.eq.iprt4 .and. j.eq.jprt4 ) then
           print *,'NDFD > MDL topo,validpt,tnew,topo,zs',validpt(i,j),tnew(i,j),topo_ndfd(i,j),zs
           print *,' pnew ',pnew(i,j),'psfc ', psfc(i,j),' thetavc ',thetavc, 't2', t2(i,j)
           print*,'pbot,zbot,tbot,qbot=',k,pbot,zbot,tbot,qbot
           print*,'frac,T,tup,tp1,tsfc,tnew=',frac,T(i,j,k),tup,tp1,tsfc,tnew(i,j)
          endif
           if (i.eq.iprt5 .and. j.eq.jprt5 ) then
           print *,'NDFD > MDL topo,validpt,tnew,topo,zs',validpt(i,j),tnew(i,j),topo_ndfd(i,j),zs
           print *,' pnew ',pnew(i,j),'psfc ', psfc(i,j),' thetavc ',thetavc, 't2', t2(i,j)
           print*,'pbot,zbot,tbot,qbot=',k,pbot,zbot,tbot,qbot
           print*,'frac,T,tup,tp1,tsfc,tnew=',frac,T(i,j,k),tup,tp1,tsfc,tnew(i,j)
          endif
           if (i.eq.iprt6 .and. j.eq.jprt6 ) then
           print *,'NDFD > MDL topo,validpt,tnew,topo,zs',validpt(i,j),tnew(i,j),topo_ndfd(i,j),zs
           print *,' pnew ',pnew(i,j),'psfc ', psfc(i,j),' thetavc ',thetavc, 't2', t2(i,j)
           print*,'pbot,zbot,tbot,qbot=',k,pbot,zbot,tbot,qbot
           print*,'frac,T,tup,tp1,tsfc,tnew=',frac,T(i,j,k),tup,tp1,tsfc,tnew(i,j)
          endif


! --- Just use q at NAM 1st level in this case.
!     should use q2, but the values dont look good
!     Obtain Td corresponding to NDFD pres otherwise.
!TEST      qv=q2(i,j)

!---> Alaska, Choose q at 1st level for more realistic output 
!     Also for CONUS....others ???
!TEST      if (gdin%region .eq. 'AK' .or. lnest) qv=q(i,j,1)
!         qv=q(i,j,1)
! Use q2, since q at 1st level could be underground [AMG]
! look at q2 to make sure it looks good
          qv=q2(i,j)

          e=pnew(i,j)/100.*qv/(0.62197+qv)
! --- dew-point temperature at original sfc
          ENL = ALOG(E)
          DWPT = (243.5*ENL-440.8)/(19.48-ENL)
          td = dwpt + 273.15
! --- dewpoint temperature
          dewnew(i,j) = min(td,tnew(i,j))
          if (k .eq. 1) then
            uc = u10(i,j)+frac * (uwnd(i,j,k)-u10(i,j))
            vc = v10(i,j)+frac * (vwnd(i,j,k)-v10(i,j))
           if (i.eq.iprt1 .and. j.eq.jprt1 ) then
             print*,'i,j,k,uc,vc,u10,v10,frac,uwnd(i,j,k)=',i,j,k,uc,vc,u10(i,j),v10(i,j),frac,uwnd(i,j,k),vwnd(i,j,k)
           endif
           if (i.eq.iprt2 .and. j.eq.jprt2 ) then
             print*,'i,j,k,uc,vc,u10,v10,frac,uwnd(i,j,k)=',i,j,k,uc,vc,u10(i,j),v10(i,j),frac,uwnd(i,j,k),vwnd(i,j,k)
           endif
           if (i.eq.iprt3 .and. j.eq.jprt3 ) then
             print*,'i,j,k,uc,vc,u10,v10,frac,uwnd(i,j,k)=',i,j,k,uc,vc,u10(i,j),v10(i,j),frac,uwnd(i,j,k),vwnd(i,j,k)
           endif
           if (i.eq.iprt4 .and. j.eq.jprt4 ) then
             print*,'i,j,k,uc,vc,u10,v10,frac,uwnd(i,j,k)=',i,j,k,uc,vc,u10(i,j),v10(i,j),frac,uwnd(i,j,k),vwnd(i,j,k)
           endif
           if (i.eq.iprt5 .and. j.eq.jprt5 ) then
             print*,'i,j,k,uc,vc,u10,v10,frac,uwnd(i,j,k)=',i,j,k,uc,vc,u10(i,j),v10(i,j),frac,uwnd(i,j,k),vwnd(i,j,k)
           endif
           if (i.eq.iprt6 .and. j.eq.jprt6 ) then
             print*,'i,j,k,uc,vc,u10,v10,frac,uwnd(i,j,k)=',i,j,k,uc,vc,u10(i,j),v10(i,j),frac,uwnd(i,j,k),vwnd(i,j,k)
           endif
          else
            uc = uwnd(i,j,k-1)+frac * (uwnd(i,j,k)-uwnd(i,j,k-1))
            vc = vwnd(i,j,k-1)+frac * (vwnd(i,j,k)-vwnd(i,j,k-1))
           if (i.eq.iprt1 .and. j.eq.jprt1 ) then
             print*,'i,j,k,uc,u10,frac,uwnd(k),uwnd(k-1)=',i,j,k,uc,u10(i,j),frac,uwnd(i,j,k),uwnd(i,j,k-1)
             print*,'i,j,k,vc,v10,frac,vwnd(k),vwnd(k-1)=',i,j,k,vc,v10(i,j),frac,vwnd(i,j,k),vwnd(i,j,k-1)
           endif
           if (i.eq.iprt2 .and. j.eq.jprt2 ) then
             print*,'i,j,k,uc,u10,frac,uwnd(k),uwnd(k-1)=',i,j,k,uc,u10(i,j),frac,uwnd(i,j,k),uwnd(i,j,k-1)
             print*,'i,j,k,vc,v10,frac,vwnd(k),vwnd(k-1)=',i,j,k,vc,v10(i,j),frac,vwnd(i,j,k),vwnd(i,j,k-1)
           endif
           if (i.eq.iprt3 .and. j.eq.jprt3 ) then
             print*,'i,j,k,uc,u10,frac,uwnd(k),uwnd(k-1)=',i,j,k,uc,u10(i,j),frac,uwnd(i,j,k),uwnd(i,j,k-1)
             print*,'i,j,k,vc,v10,frac,vwnd(k),vwnd(k-1)=',i,j,k,vc,v10(i,j),frac,vwnd(i,j,k),vwnd(i,j,k-1)
           endif
           if (i.eq.iprt4 .and. j.eq.jprt4 ) then
             print*,'i,j,k,uc,u10,frac,uwnd(k),uwnd(k-1)=',i,j,k,uc,u10(i,j),frac,uwnd(i,j,k),uwnd(i,j,k-1)
             print*,'i,j,k,vc,v10,frac,vwnd(k),vwnd(k-1)=',i,j,k,vc,v10(i,j),frac,vwnd(i,j,k),vwnd(i,j,k-1)
           endif
           if (i.eq.iprt5 .and. j.eq.jprt5 ) then
             print*,'i,j,k,uc,u10,frac,uwnd(k),uwnd(k-1)=',i,j,k,uc,u10(i,j),frac,uwnd(i,j,k),uwnd(i,j,k-1)
             print*,'i,j,k,vc,v10,frac,vwnd(k),vwnd(k-1)=',i,j,k,vc,v10(i,j),frac,vwnd(i,j,k),vwnd(i,j,k-1)
           endif
           if (i.eq.iprt6 .and. j.eq.jprt6 ) then
             print*,'i,j,k,uc,u10,frac,uwnd(k),uwnd(k-1)=',i,j,k,uc,u10(i,j),frac,uwnd(i,j,k),uwnd(i,j,k-1)
             print*,'i,j,k,vc,v10,frac,vwnd(k),vwnd(k-1)=',i,j,k,vc,v10(i,j),frac,vwnd(i,j,k),vwnd(i,j,k-1)
           endif
          endif

! -- 0.7 factor is a wag at surface effects on wind speed
!     when interpolating from the free atmosphere to
!     the NDFD topo.
          if (ispdsfc .eq. 1) then
            speedc = 0.7*sqrt(uc*uc+vc*vc)
            speed = sqrt(uc**2 + vc**2)
            ratio = max(1.,speedc/(max(0.001,speed)) )
            unew(i,j) = uc
            vnew(i,j) = vc
          endif

!============================================
        ENDIF
!============================================

120     continue

!      Adjust  winds to topography
      if (CVADJ.eq.'T')                           &
      call vadjust(validpt,veg_ndfd,unew,vnew,topo_ndfd,dx,dy,im,jm,lm,gdin)

!============================================
! -- use land mask to get better temps/dewpoint/winds
!      near coastlines.
!    Use nearest neighbor adjustment where NAM
!      land-water mask does not mask NDFD land-water mask 
!============================================

!  create temporary holder for u,v,t,td so that the "real"
!   values don't get shifted around in the adjustment
       ttmp=tnew
       dtmp=dewnew
       utmp=unew
       vtmp=vnew 
       rough_mod = veg_nam_ndfd

       do j=1,jm
       do i=1,im
           if (i.eq.iprt1 .and. j.eq.jprt1 ) then
             print*,'i,j,unew,vnew=',i,j,unew(i,j),vnew(i,j)
           endif
           if (i.eq.iprt2 .and. j.eq.jprt2 ) then
             print*,'i,j,unew,vnew=',i,j,unew(i,j),vnew(i,j)
           endif
           if (i.eq.iprt3 .and. j.eq.jprt3 ) then
             print*,'i,j,unew,vnew=',i,j,unew(i,j),vnew(i,j)
           endif
           if (i.eq.iprt4 .and. j.eq.jprt4 ) then
             print*,'i,j,unew,vnew=',i,j,unew(i,j),vnew(i,j)
           endif
           if (i.eq.iprt5 .and. j.eq.jprt5 ) then
             print*,'i,j,unew,vnew=',i,j,unew(i,j),vnew(i,j)
           endif
           if (i.eq.iprt6 .and. j.eq.jprt6 ) then
             print*,'i,j,unew,vnew=',i,j,unew(i,j),vnew(i,j)
           endif
       if(tnew(i,j).ge.spval)print*,'i,j,tnew=',i,j,tnew(i,j)
       enddo
       enddo

       print*, ' min/max of rough_mod:  ', minval(rough_mod),maxval(rough_mod)


! ----------------------------------------------------
! -- Adjust to rough_mod iteratively for land to water
! ----------------------------------------------------
       do k=1,15
       nmod = 0
       do j=1,jm
        jm1 = max(1,j-1)
        jp1 = min(jm,j+1)
       do i=1,im
        im1 = max(1,i-1)
        ip1 = min(im,i+1)
        ladjland=.false.
        if (lvegtype .and. veg_ndfd(i,j).eq.veglim) ladjland=.true.  ! veg_ndfd = 16 (ndfd=water)
        if (.not.lvegtype .and. veg_ndfd(i,j).lt.veglim) ladjland=.true.  ! veg_ndfd < 0.5

        if (ladjland) then
         if(rough_mod(i,j).gt. rghlim) then                       ! mdl>0,land
          if(any(rough_mod(im1:ip1,jm1:jp1).lt.rghlim)) then 
             if(validpt(i,j)) then
              rough_mod(i,j) = 0.0
              nmod(1) = nmod(1) + 1
             endif
           end if
          end if
        else
          if(rough_mod(i,j).lt.rghlim) then
           if(any(rough_mod(im1:ip1,jm1:jp1).gt.rghlim)) then
            if (validpt(i,j)) then      ! Added 03-13-13
             rough_mod(i,j) = 0.1*scale
             nmod(2) = nmod(2) + 1
            endif
           end if
          end if
        end if
        end do
        end do
        write (6,*)k,' No. pts changed land-to-water',nmod(1)
        write (6,*)k,' No. pts changed water-to-land',nmod(2)
       end do

       do j=1,jm
       do i=1,im
         if (veg_nam_ndfd(i,j).gt.rghlim) then  ! mdl> 0 , land
          if (rough_mod(i,j).lt.rghlim) then    ! ndfd = 0, water
! -----------------------------------------------------------------
! -- i.e.  NDFD grid-point is over WATER (per rough_mod)
!          NAM-interp grid-point is over LAND
! -----------------------------------------------------------------
               
          do ibuf=1,10
           ia = max(1,i-ibuf)
           ib = min(im,i+ibuf)
           ja = max(1,j-ibuf)
           jb = min(jm,j+ibuf)
               
            do jw = ja,jb
            do iw = ia,ib
              if (veg_nam_ndfd(iw,jw).lt.rghlim) then  ! mdl = 0, leave loop
                if(validpt(iw,jw)) then
                 unew(i,j) = utmp(iw,jw)
                 vnew(i,j) = vtmp(iw,jw)
                 tnew(i,j) = ttmp(iw,jw)
                 dewnew(i,j) = dtmp(iw,jw)
                 n_rough_yes = n_rough_yes+1
                 goto 883  ! check if leaves all three loops
                endif
              end if
            end do
            end do
               
          end do
              
 883      continue 
    if (i.eq.iprt.and. j.eq.jprt)print *,'z0-n >.05',validpt(i,j),tnew(i,j), &
     ' NDFD topo ',topo_ndfd(i,j),' MDL topo ',zs,veg_nam_ndfd(i,j),rghlim
          end if 
         end if 
! -----------------------------------------------------------------
! -- i.e.  NDFD grid-point is over LAND (per rough_mod)
!          NAM-interp grid-point is over WATER
! -----------------------------------------------------------------
!          what about mdl masks where water=16
!          if (lvegtype .and. veg_ndfd(i,j).eq.veglim) ladjland=.true.  ! veg_ndfd = 16 (ndfd=water)

          if (veg_nam_ndfd(i,j).lt.rghlim .and. rough_mod(i,j).gt.rghlim) then  ! mdl=0 (water) ;ndfd>0 (land)
               
           do ibuf=1,10
            ia = max(1,i-ibuf)
            ib = min(im,i+ibuf)
            ja = max(1,j-ibuf)
            jb = min(jm,j+ibuf)
               
            do jw = ja,jb
            do iw = ia,ib
              if (veg_nam_ndfd(iw,jw).gt.rghlim) then  ! mdl = land
                if(validpt(iw,jw)) then
                 unew(i,j) = utmp(iw,jw)
                 vnew(i,j) = vtmp(iw,jw)
                 tnew(i,j) = ttmp(iw,jw)
                 dewnew(i,j) = dtmp(iw,jw)
                 m_rough_yes = m_rough_yes+1
                 goto 783 
                endif
              end if
            end do
            end do
           end do
 783 continue
         end if
    if (i.eq.iprt.and. j.eq.iprt)print *,'z0n<.05',validpt(i,j),tnew(i,j), &
     topo_ndfd(i,j),zs,veg_nam_ndfd(i,j),rghlim,rough_mod(i,j)

       end do
       end do
       print *,'TNEW ',minval(tnew),maxval(tnew)
       print *,'PNEW ',minval(pnew),maxval(pnew)


!      Adjust dewpoint to downscaled  sfc pressure
       where(validpt)  
         where (dewnew.lt.spval) &
         qnew=PQ0/PSFC*EXP(A2*(dewnew-A3)/(dewnew-A4))
       endwhere
       do j=1,jm
       do i=1,im
           if (i.eq.iprt1 .and. j.eq.jprt1 ) then
             print*,'i,j,u10,v10,t2,d2,q2=',i,j,u10(i,j),v10(i,j),t2(i,j),d2(i,j),q2(i,j)
             print*,'i,j,unew,vnew,tnew,dewnew,qnew=',i,j,unew(i,j),vnew(i,j),tnew(i,j),dewnew(i,j),qnew(i,j)
           endif
           if (i.eq.iprt2 .and. j.eq.jprt2 ) then
             print*,'i,j,u10,v10,t2,d2,q2=',i,j,u10(i,j),v10(i,j),t2(i,j),d2(i,j),q2(i,j)
             print*,'i,j,unew,vnew,tnew,dewnew,qnew=',i,j,unew(i,j),vnew(i,j),tnew(i,j),dewnew(i,j),qnew(i,j)
           endif
           if (i.eq.iprt3 .and. j.eq.jprt3 ) then
             print*,'i,j,u10,v10,t2,d2,q2=',i,j,u10(i,j),v10(i,j),t2(i,j),d2(i,j),q2(i,j)
             print*,'i,j,unew,vnew,tnew,dewnew,qnew=',i,j,unew(i,j),vnew(i,j),tnew(i,j),dewnew(i,j),qnew(i,j)
           endif
           if (i.eq.iprt4 .and. j.eq.jprt4 ) then
             print*,'i,j,u10,v10,t2,d2,q2=',i,j,u10(i,j),v10(i,j),t2(i,j),d2(i,j),q2(i,j)
             print*,'i,j,unew,vnew,tnew,dewnew,qnew=',i,j,unew(i,j),vnew(i,j),tnew(i,j),dewnew(i,j),qnew(i,j)
           endif
           if (i.eq.iprt5 .and. j.eq.jprt5 ) then
             print*,'i,j,u10,v10,t2,d2,q2=',i,j,u10(i,j),v10(i,j),t2(i,j),d2(i,j),q2(i,j)
             print*,'i,j,unew,vnew,tnew,dewnew,qnew=',i,j,unew(i,j),vnew(i,j),tnew(i,j),dewnew(i,j),qnew(i,j)
           endif
           if (i.eq.iprt6 .and. j.eq.jprt6 ) then
             print*,'i,j,u10,v10,t2,d2,q2=',i,j,u10(i,j),v10(i,j),t2(i,j),d2(i,j),q2(i,j)
             print*,'i,j,unew,vnew,tnew,dewnew,qnew=',i,j,unew(i,j),vnew(i,j),tnew(i,j),dewnew(i,j),qnew(i,j)
           endif
        enddo
        enddo
       return
       end
