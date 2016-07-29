   PROGRAM SMARTINIT
     use constants          ! Define constants used by NDFDgrid.f90
     use grddef             ! Initialize grid and run params
     use aset3d             ! Define 3-d grids
     use aset2d             ! Define 2-d grids
     use asetdown           ! Define downscaled output grids 
     use rdgrib             ! Define grib read routines rdhdrs, setvar
     USE GRIB_MOD
     USE pdstemplates

!========================================================================
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    SMARTINIT    CREATES NDFD FILES 
!   PRGRMMR: MANIKIN           ORG: W/NP22     DATE: 07-08-06

! ABSTRACT:   THIS CODE TAKES NATIVE NAM/GFS/HRW FILES AND GENERATES
!          2.5 or 5 KM OUTPUT CONTAINING NDFD ELEMENTS
!  Input : RH (0-1), T at 950,850,700 and 500 mb
! PROGRAM HISTORY LOG:
!   07-08-06  G MANIKIN  - COMPLETED ADAPTING CODE TO NAM 
!   12-11-30  J McQueen  - Converted to f90, unified for different domains
!========================================================================
      INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200),ID(25)
      INTEGER IMAX,JMAX,KMAX,FHR,CYC,DATE,HOUR,ITOT,OGRD,NARGC,HAVESREF

      LOGICAL RITEHD,LCYCON,LHR3,LHR12,LNEST,LHIRESW
      CHARACTER*4 CTMP,REGION,CORE

      CHARACTER*50, ALLOCATABLE :: WXSTRING(:,:)
      CHARAcTER(LEN=80) :: FNAMEOUT, FNAME2OUT, FNAME
      CHARACTER*10 DESCR2
      CHARACTER*4 RESTHR
      CHARACTER*1  CDOT
!-----------------------------------------------------------------------------------
!  TYPE(ISET), INTENT(IN) :: iprcp(,:,)
   INTEGER, ALLOCATABLE :: ISNOW(:,:),IZR(:,:),IIP(:,:),IRAIN(:,:)

!  TYPE PCPSET, INTENT(INOUT) :: prcp 
   REAL,    ALLOCATABLE :: QPF3(:,:),P03M(:,:),QPF6(:,:),P06M(:,:)
   REAL,    ALLOCATABLE :: QPF12(:,:),P12M(:,:),SN03(:,:),SN06(:,:)
   REAL,    ALLOCATABLE :: POP3(:,:),POP6(:,:),POP12(:,:),SNOWAMT3(:,:)
   REAL,    ALLOCATABLE :: CWR(:,:),SKY(:,:),WETFRZ(:,:),SNOWAMT6(:,:)
   REAL,    ALLOCATABLE :: GRIDWX(:,:)

!  TYPE PROB PCP
   REAL,    ALLOCATABLE :: P3CP01(:,:),P3CP10(:,:),P3CP50(:,:)
   REAL,    ALLOCATABLE :: P6CP01(:,:),P6CP10(:,:),P6CP50(:,:)
   REAL,    ALLOCATABLE :: P12CP01(:,:),P12CP10(:,:),P12CP50(:,:)

   REAL,    ALLOCATABLE :: THOLD(:,:,:),DHOLD(:,:,:),TMAX12(:,:),TMIN12(:,:)
   REAL,    ALLOCATABLE :: TMAX3(:,:),TMIN3(:,:),RHMAX12(:,:),RHMIN12(:,:)
   REAL,    ALLOCATABLE :: RHMAX3(:,:),RHMIN3(:,:)

!  USED in MAIN only
   REAL,    ALLOCATABLE :: DIRTRANS(:,:),MGTRANS(:,:),LAL(:,:),MIXHGT(:,:)
   REAL,    ALLOCATABLE :: TEMP1(:,:),TEMP2(:,:)
   INTEGER, ALLOCATABLE :: HAINES(:,:),HLVL(:,:)

   LOGICAL, ALLOCATABLE :: VALIDPT(:,:)
!
!   REAL,    ALLOCATABLE   :: GRID(:)
   TYPE (GINFO) :: GDIN
   TYPE (GRIBFIELD):: GFLD, GFLD8

    INCLUDE 'DEFGRIBINT.INC'   ! interface statements for gribit subroutines
!-----------------------------------------------------------------------------------------
    INTERFACE
    SUBROUTINE GETGRIB(ISNOW,IZR,IIP,IRAIN,VEG,WETFRZ,  &
    P03M,P06M,P12M,SN03,SN06,S3REF01,S3REF10,S3REF50,S6REF01,  &
    S6REF10,S6REF50,S12REF01,S12REF10,S12REF50, THOLD,DHOLD,GDIN,VALIDPT, &
    GFLD,GFLD8,HAVESREF)
    use grddef
    use aset3d
    use aset2d
    use rdgrib
    use constants
    USE GRIB_MOD
    USE pdstemplates

    TYPE (GINFO) :: GDIN
      TYPE (GRIBFIELD):: GFLD, GFLD8
    INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)
    INTEGER YEAR,MON,DAY,IHR,DATE,FHR,IFHR,IFHRIN,IFHRSTR,HAVESREF
    PARAMETER(MBUF=2000000)
    CHARACTER CBUF(MBUF)
    CHARACTER*80 FNAME
    CHARACTER*4 DUM1, REGION, CORE
    LOGICAL*1 LCYCON,LHR3,LHR6,LHR12,LFULL,LANL,LLIMITED,LNEST,LHIRESW
    INTEGER JENS(200),KENS(200),CYC
   INTEGER, INTENT(INOUT) :: ISNOW(:,:),IZR(:,:),IIP(:,:),IRAIN(:,:)
   REAL,    INTENT(INOUT) :: P03M(:,:),P06M(:,:),P12M(:,:),SN03(:,:),SN06(:,:)
   REAL,    INTENT(INOUT) :: WETFRZ(:,:)
   REAL,    INTENT(INOUT) :: THOLD(:,:,:),DHOLD(:,:,:)
   REAL,    INTENT(INOUT) :: VEG(:,:)
   REAL,    INTENT(INOUT) :: S3REF01(:,:),S3REF10(:,:),S3REF50(:,:)
   REAL,    INTENT(INOUT) :: S6REF01(:,:),S6REF10(:,:),S6REF50(:,:)
   REAL,    INTENT(INOUT) :: S12REF01(:,:),S12REF10(:,:),S12REF50(:,:)
   LOGICAL, INTENT(INOUT) :: VALIDPT(:,:)
   END SUBROUTINE getgrib
!--------------------------------------------------------------------------------------
   SUBROUTINE SKYCVR(SKY,CFR,GDIN)
        use grddef
        REAL TSKY(7)
        TYPE (GINFO),INTENT(IN) :: GDIN
        REAL,    INTENT(IN)     :: CFR(:,:,:)
        REAL,    INTENT(INOUT)  :: SKY(:,:)
   END SUBROUTINE skycvr

   SUBROUTINE SNOWFALL(SN0,SNOWAMT,DOWNT,THOLD,GDIN,AVG,VALIDPT)
        use grddef
        TYPE (GINFO), INTENT(IN) :: GDIN
        REAL,    INTENT(IN)    :: DOWNT(:,:), THOLD(:,:,:),SN0(:,:)
        REAL,    INTENT(INOUT) :: SNOWAMT(:,:)
        REAL,    ALLOCATABLE   :: TEMP1(:,:),TEMP2(:,:)
        LOGICAL, INTENT(IN)    :: VALIDPT(:,:)
   END SUBROUTINE snowfall

   SUBROUTINE MKPOP(PBLMARK,RH,BLI,PCP01,PCP10,PXCP01,PXCP10,QPF,POP,GDIN,IAHR,VALIDPT)
        use grddef
        TYPE (GINFO), INTENT(IN) :: GDIN
        REAL,    INTENT(IN)    :: PBLMARK(:,:), RH(:,:,:),BLI(:,:),QPF(:,:)
        REAL,    INTENT(INOUT) :: PCP01(:,:),PCP10(:,:),PXCP01(:,:),PXCP10(:,:)
        REAL,    INTENT(OUT)   :: POP(:,:)
        REAL,    ALLOCATABLE   :: TMPPCP(:,:)
        LOGICAL, INTENT(IN)    :: VALIDPT(:,:)
   END SUBROUTINE mkpop

   SUBROUTINE BOUND(FLD,FMIN,FMAX)
        REAL FMAX, FMIN
        REAL,    INTENT(INOUT) :: FLD(:,:)
   END SUBROUTINE bound

   SUBROUTINE MAKESTRING(IRAIN,ISNOW,IZR,IIP,LIFT,PROB,GDIN,STRING,GRIDWX,VALIDPT)
       use grddef
      TYPE (GINFO), INTENT(IN) :: GDIN
      INTEGER,    INTENT(IN) :: IRAIN(:,:),ISNOW(:,:),IZR(:,:),IIP(:,:)
      REAL,       INTENT(IN) :: LIFT(:,:),PROB(:,:)
      REAL,      INTENT(INOUT) ::  GRIDWX(:,:)
      LOGICAL,   INTENT(IN)  :: VALIDPT(:,:)
      CHARACTER*50, INTENT(INOUT) :: STRING(:,:)
      CHARACTER*50  ,TRACK(200),TEMP
      REAL,      ALLOCATABLE ::  WX(:,:),THUNDER(:,:)
      INTEGER START
   END SUBROUTINE makestring 

  subroutine NDFDgrid(veg_nam_ndfd,tnew,dewnew,unew,vnew, &
      qnew,pnew,topo_ndfd,veg_ndfd,gdin,validpt)
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

 INTERFACE
    SUBROUTINE vadjust(VALIDPT,VEG_NDFD,U,V,HTOPO,DX,DY,IM,JM,gdin)
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
    
   END SUBROUTINE ndfdgrid 

   SUBROUTINE GRIBLIMITED(SKY,IUNIT,GDIN,GFLD,IM,JM)
      use grddef
      use aset2d
      use asetdown
      USE GRIB_MOD
       INTEGER ID(25)
       LOGICAL RITEHD
       TYPE (GINFO) :: GDIN
       REAL,    INTENT(INOUT)  :: SKY(:,:)
   TYPE (GRIBFIELD):: GFLD, GFLD8
   END SUBROUTINE griblimited

   SUBROUTINE HINDEX (IM,JM,HAINES,HLVL,VALIDPT)
      use aset2d
      use asetdown
      LOGICAL, INTENT(IN)   :: VALIDPT(:,:)
      INTEGER, INTENT(INOUT)    :: HAINES(:,:),HLVL(:,:)
   END SUBROUTINE hindex

   FUNCTION CalcQ(ptmp,ttmp)
      use constants
      REAL, INTENT(IN) :: ptmp,ttmp
   END FUNCTION calcq

   FUNCTION calcw(qpftmp,lmbl,rhavg,qpfmax,RHexcess,adjAmount)
    REAL :: calcw
    REAL, INTENT(IN) :: qpftmp,rhavg,qpfmax,RHexcess,adjAmount
    INTEGER, INTENT(IN) :: lmbl
   END FUNCTION calcw

   END INTERFACE
!-----------------------------------------------------------------------------------------
      LNEST=.FALSE.
      LHIRESW=.FALSE.
!     LCYCON=FALSE;LHR12=.FALSE.;LHR3=.FALSE.
      LCYCON=.FALSE.;LHR12=.FALSE.;LHR3=.FALSE.
      nargc=iargc()
      call getarg(1,CTMP)
      READ (ctmp,*) GDIN%CYC
      call getarg(2,CTMP)
      READ (ctmp,*) GDIN%FHR
      call getarg(3,CTMP)
      READ (ctmp,*) GDIN%OGRD
      call getarg(4,GDIN%REGION)
      call getarg(5,ctmp)
      READ (ctmp,*) INEST
      if(inest.gt.0)GDIN%LNEST=.TRUE.
      call getarg(6,ctmp)
      READ (ctmp,*) GDIN%INHRFRQ   !hrly freq of input files (eg 1 or 3 hrly)
      IFHRSTR=0
      if (nargc.gt.6) call getarg(7,ctmp)
      READ (ctmp,*) GDIN%IFHRSTR   !starting hour (0 or 87 for dgex)
      call getarg(8,GDIN%CORE)  ! For hiresw core nmmb or arw
      
      FHR=GDIN%FHR;IFHR=FHR;IFHRIN=FHR;REGION=GDIN%REGION;OGRD=GDIN%OGRD
      CYC=GDIN%CYC;LNEST=GDIN%LNEST;IFHRSTR=GDIN%IFHRSTR;CORE=GDIN%CORE
      INHRFRQ=GDIN%INHRFRQ
      if (cyc .ne. 00 .and. cyc .ne. 06 .and. cyc .ne. 12 .and. cyc .ne.18)then
        HAVESREF=0
      else
        HAVESREF=1
      endif
      print*,'CYC, HAVESREF=',cyc, havesref
      if (CORE.eq.'nmmb'.or. CORE.eq.'arw') GDIN%LHIRESW=.true.
      LHIRESW=GDIN%LHIRESW
      print *,  nargc,' Running Smartinit for FHR', FHR,' IFHRSTR ',IFHRSTR
      print *, 'RUN CYCLE ', CYC
      print *, 'REGION ',TRIM(REGION)
      print *, 'OUTPUT GRID # ',OGRD
      print *, 'LNEST ',LNEST, ' INPUT FILE FREQ ',INHRFRQ,' HRS'
      print *, 'CORE ', trim(CORE), 'LCYCON ', LCYCON

      FHR3=FHR-3
      FHR6=FHR-6
      FHR12=FHR-12

      IF (CYC.EQ.12.OR.CYC.EQ.00) LCYCON=.TRUE.
      IF(MOD(FHR,3).EQ.0) LHR3=.TRUE.
      IF((FHR-IFHRSTR).GE.12.and.LCYCON.AND.MOD(FHR,12).EQ.0) LHR12=.TRUE.
      IF(.NOT.LCYCON) THEN
        IF((FHR-IFHRSTR).GT.6 .AND. MOD(FHR-6,12).EQ.0) LHR12=.TRUE.
      ENDIF
      GDIN%LCYCON=LCYCON;GDIN%LHR12=LHR12

! READ THE GRIB FILES FROM THE NAM AND SREF.  WE NEED TO READ A
!   FULL COMPLEMENT OF DATA EVERY 3 HOURS.  FOR THE IN-BETWEEN
!   FCST HOURS, WE ONLY NEED TO KEEP TRACK OF DOWNSCALED TEMP
!   AND DEW POINT (FOR MIN/MAX PURPOSES), SO WE NEED ONLY A VERY
!   LIMITED AMOUNT OF DATA.   FOR THE ANALYSIS TIME, WE NEED A
!   SPECIAL CALL OF THE FULL DATA SET BUT WITHOUT PRECIP

!JTM    GETGRIB: Combined off and on-cycle runs (removed GETGRIB_OFF)
!JTM             Combined for limited off-hour reads (removed
!JTM                                                  GETGRIB_LIMITED)
!JTM             Combined for fhr=0 analysis run (removed GETGRIB_ANL)

!==========================================================
!     READ INDEX FILE TO GET GRID SPECS
!==========================================================
    LUGB=11;LUGI=12
!   CALL RDHDRS(LUGB,LUGI,IGDNUM,GDIN,NUMVAL)
      write(0,*) 'to RDHDRS_g2 call'
      CALL RDHDRS_g2(LUGB,LUGI,IGDNUM,GDIN,NUMVAL)
    IM=GDIN%IMAX;JM=GDIN%JMAX;ITOT=NUMVAL
    if (lnest) then   
      GDIN%KMAX=40
      if (trim(CORE).eq.'GFS') GDIN%KMAX=64
    else
      GDIN%KMAX=60       ! HARDWIRE MAXLEVs hybrid level files
      if (.not. LHR3) GDIN%KMAX=35  ! non-nests inbetween hrs after 54/60 hrs
! Fix Total cloud bug for hourly files; found in reanalysis work (AMG)
      if (TRIM(REGION).EQ.'AKRT') GDIN%KMAX=60
    endif

    KMAX=GDIN%KMAX

   ALLOCATE (THOLD(IM,JM,12),DHOLD(IM,JM,12),STAT=kret)
   ALLOCATE (ZSFC(IM,JM),STAT=kret)
   ALLOCATE (PSFC(IM,JM),STAT=kret)
   ALLOCATE (ISNOW(IM,JM),IIP(IM,JM),IZR(IM,JM),IRAIN(IM,JM),STAT=kret)
   ALLOCATE (WETFRZ(IM,JM),VIS(IM,JM),T2(IM,JM),Q2(IM,JM),STAT=kret)
   ALLOCATE (D2(IM,JM),U10(IM,JM),V10(IM,JM),BLI(IM,JM),STAT=kret)
   ALLOCATE (VEG(IM,JM),VEG_NDFD(IM,JM),STAT=kret)
   ALLOCATE (P03M(IM,JM),SN03(IM,JM),STAT=kret)
   ALLOCATE (P06M(IM,JM),SN06(IM,JM),STAT=kret)
   ALLOCATE (P12M(IM,JM),STAT=kret)
   ALLOCATE (PMID(IM,JM,KMAX),HGHT(IM,JM,KMAX),STAT=kret)
   ALLOCATE (T(IM,JM,KMAX),Q(IM,JM,KMAX),STAT=kret)
   ALLOCATE (UWND(IM,JM,KMAX),VWND(IM,JM,KMAX),STAT=kret)
   ALLOCATE (CFR(IM,JM,KMAX),CWR(IM,JM),STAT=kret)
   ALLOCATE (T950(IM,JM),T850(IM,JM),T700(IM,JM),T500(IM,JM),STAT=kret)
   ALLOCATE (RH850(IM,JM),RH700(IM,JM),STAT=kret)
   ALLOCATE (GUST(IM,JM),REFC(IM,JM),STAT=kret)
   ALLOCATE (P3CP01(IM,JM),P3CP10(IM,JM),P3CP50(IM,JM),STAT=kret)
   ALLOCATE (P6CP01(IM,JM),P6CP10(IM,JM),P6CP50(IM,JM),STAT=kret)
   ALLOCATE (P12CP01(IM,JM),P12CP10(IM,JM),P12CP50(IM,JM),STAT=kret)
   ALLOCATE (HAINES(IM,JM),HLVL(IM,JM),STAT=kret)
   ALLOCATE (CEIL(IM,JM),SLP(IM,JM),SST(IM,JM),STAT=kret)
!  for nests
   ALLOCATE (VALIDPT(IM,JM),STAT=kret)
   VALIDPT=.TRUE.
   if(lnest) ALLOCATE (LCLD(IM,JM),MCLD(IM,JM),HCLD(IM,JM),TCLD(IM,JM),STAT=kret)

    RH=0.
    print*,'CALL GETGRIB with HAVESREF: ', HAVESREF
    CALL GETGRIB(ISNOW,IZR,IIP,IRAIN,VEG,WETFRZ,  &
    P03M,P06M,P12M,SN03,SN06,P3CP01,P3CP10,P3CP50,P6CP01,  &
    P6CP10,P6CP50,P12CP01,P12CP10,P12CP50, THOLD,DHOLD,GDIN,VALIDPT, &
    GFLD,GFLD8,HAVESREF)
        write(0,*) 'GFLD%igdtmpl(8): ', GFLD%igdtmpl(8)
        write(0,*) 'GFLD%igdtmpl(9): ', GFLD%igdtmpl(9)

!!! Reset VEG here (Matt Pyle, 1/14)
        print *,'VEG ',minval(veg),maxval(veg)
        if (CORE .eq. 'nmmb') then
          
          print *,'MODEL CORE  ', CORE, ' ADJUSTING VEG'
!TEST          where (VEG .le. 0.) VEG=16.
          print *,'VEG ',minval(veg),maxval(veg)
        endif

    print *,'MAIN VALIDPT, Temperature ',validpt(50,50),T(50,50,1)

!   Initialize varbs to spval (for nests)
    where (.not. validpt)
      PSFC=SPVAL;REFC=SPVAL;WETFRZ=SPVAL;VIS=SPVAL;CEIL=SPVAL;SLP=SPVAL;SST=SPVAL
      P03M=SPVAL;P06M=SPVAL;P12M=SPVAL;CWR=SPVAL
    endwhere


!  CALL THE DOWNSCALING CODE 

       ALLOCATE (DOWNT(IM,JM),DOWNDEW(IM,JM),STAT=kret)
       ALLOCATE (DOWNU(IM,JM),DOWNV(IM,JM),STAT=kret)
       ALLOCATE (DOWNQ(IM,JM),TOPO(IM,JM),STAT=kret)
       ALLOCATE (DOWNP(IM,JM),STAT=kret)
       ALLOCATE (WGUST(IM,JM),PBLMARK(IM,JM),STAT=kret)
       ALLOCATE (TEMP1(IM,JM),TEMP2(IM,JM),STAT=kret)

       CALL NDFDgrid(VEG,DOWNT,DOWNDEW,DOWNU,DOWNV,DOWNQ,DOWNP,TOPO,VEG_NDFD,gdin,VALIDPT)

!      Compute WGUST at all forecast hours to write out for RTMA 
       if (.not.lhiresw) then
!        IF (FHR .LE. 12 .or. MOD(FHR,3).EQ.0)THEN
         IF (FHR .LE. 12 .or. MOD(FHR,3).EQ.0 .or. (FHR .LE. 36 .and.  TRIM(REGION).EQ.'CS2P' ))THEN
           WGUST=SPVAL;TEMP1=SPVAL
           where(validpt)
             TEMP1=SQRT(DOWNU*DOWNU+DOWNV*DOWNV)
             WGUST=MAX(GUST,TEMP1)
           endwhere
           WGUST=MIN(WGUST,SPVAL)
           print *, 'WGUST',minval(wgust),maxval(wgust)
         ENDIF
       endif

!      OUTPUT FULL Downscaled fields every 3 forecast hours
!=================================================
       IF (MOD(FHR,3).EQ.0) THEN 
!=================================================

         print *, 'OUTPUT  3-hrly downscaled Varibles',FHR

         ! MODIFY HERE to write as proper level?

!! "2 m" fields - but write out as surface fields, since original smartinit does that
!
       ID(9)=105
       ID(11)=2
!
       RITEHD = .TRUE.
       ID(1:25) = 0
       ID(8)=11
       ID(9)=105
       ID(11)=2
       DEC=-2.0
!! need to use a GRIBI2 routine like is available in grib2_module

!      FNAMEOUT='smartg2.xx'
       FNAME='MESO'//TRIM(REGION)
       RESTHR='tm00'
       CDOT='.'
       WRITE(DESCR2,'(I2.2)') FHR
       IF (FHR.GE.100) WRITE(DESCR2,'(I3.3)') FHR

        write(0,*) 'FHR known as: ', FHR
!      WRITE(FNAMEOUT(9:10),FMT='(I2.2)')FHR
       FNAMEOUT = TRIM(FNAME)//TRIM(DESCR2)//CDOT//RESTHR
!       write(0,*) 'FNAMEOUT(1:10): ', FNAMEOUT(1:10)
        write(0,*) 'FNAMEOUT: ', TRIM(FNAMEOUT)

       CALL BAOPEN(70,FNAMEOUT,IRET)
        write(0,*) 'IRET from BAOPEN of 70: ', IRET

        NUMV=IM*JM

!       do J=1,JM
!       do I=1,IM

!       if (validpt(I,J)) then

!       if (DOWNT(I,J) .le. 200.) then
!       write(0,*) 'bad small DOWNT: ', I,J, DOWNT(I,J)
!       DOWNT(I,J)=230.
!       endif

!       if (DOWNT(I,J) .ge. 330.) then
!       write(0,*) 'bad large DOWNT: ', I,J, DOWNT(I,J)
!       DOWNT(I,J)=310.
!       endif

!       endif

!       enddo
!       enddo

        CALL FILL_FLD(GFLD,NUMV,IM,JM,DOWNT)

       GFLD%ipdtnum=0
       GFLD%ipdtmpl(1)=0
       GFLD%ipdtmpl(2)=0
       GFLD%ipdtmpl(9)=gdin%FHR
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0

       GFLD%idrtnum=40 ! 40 = JPEG
       GFLD%idrtmpl(5)=0
       GFLD%idrtmpl(6)=0
       GFLD%idrtmpl(7)=-1

       gfld%idrtmpl(1)=0


       CALL set_scale(gfld, DEC)
        write(0,*) 'back from set_scale'
       CALL PUTGB2(70,GFLD,IRET) ! DOWNT
         print *, 'DOWNT',minval(downt),maxval(downt)

! ----------------------------------------

       DEC=-2.0

       CALL FILL_FLD(GFLD,NUMV,IM,JM,DOWNDEW)

       GFLD%ipdtmpl(1)=0
       GFLD%ipdtmpl(2)=6
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0
       gfld%idrtmpl(2)=DEC

       CALL set_scale(gfld, DEC)
       CALL PUTGB2(70,GFLD,IRET) ! DOWNDEW
         print *, 'DOWNDEW',minval(downdew),maxval(downdew)

! ----------------------------------------

! ----------------------------------------

! original smartinit has DEC=3.0 - should I keep it? Matt set precision to 6.0
       DEC=3.0
!      DEC=6.0

!       do J=1,JM
!       do I=1,IM

!       if (validpt(I,J)) then

! Due to precision, sometimes Q is set to zero.

!       if (DOWNQ(I,J) .le. 1.e-12) then
!       write(0,*) 'bad DOWNQ: ', I,J, DOWNQ(I,J)
!       DOWNQ(I,J)=1.e-8
!       endif

!       if (DOWNQ(I,J) .ge. 50.e-3) then
!       write(0,*) 'bad large DOWNQ: ', I,J, DOWNQ(I,J)
!       DOWNQ(I,J)=10.e-3
!       endif

!       endif

!       enddo
!       enddo

       CALL FILL_FLD(GFLD,NUMV,IM,JM,DOWNQ)

       GFLD%ipdtmpl(1)=1
       GFLD%ipdtmpl(2)=0
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0

        write(0,*) 'scale and write DOWNQ'
       CALL set_scale(gfld, DEC)
       CALL PUTGB2(70,GFLD,IRET)  ! DOWNQ
         print *, 'DOWNQ',minval(downq),maxval(downq)

! ----------------------------------------

       DEC=-2.0

       CALL FILL_FLD(GFLD,NUMV,IM,JM,DOWNU)

       GFLD%ipdtmpl(1)=2
       GFLD%ipdtmpl(2)=2
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0

       CALL set_scale(gfld, DEC)
       CALL PUTGB2(70,GFLD,IRET) ! DOWNU
         print *, 'DOWNU',minval(downu),maxval(downu)

! ----------------------------------------

       DEC=-2.0

       CALL FILL_FLD(GFLD,NUMV,IM,JM,DOWNV)

       GFLD%ipdtmpl(1)=2
       GFLD%ipdtmpl(2)=3
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0
        write(0,*) 'gfld%idrtmpl: ', gfld%idrtmpl

       CALL set_scale(gfld, DEC)
       CALL PUTGB2(70,GFLD,IRET) ! DOWNV
         print *, 'DOWNV',minval(downv),maxval(downv)

! ----------------------------------------

       DEC=3.0

       CALL FILL_FLD(GFLD,NUMV,IM,JM,WGUST)

       GFLD%discipline=0

       GFLD%ipdtmpl(1)=2
       GFLD%ipdtmpl(2)=22
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0

       CALL set_scale(gfld, DEC)
       CALL PUTGB2(70,GFLD,IRET) ! WGUST
         print *, 'GUST',minval(wgust),maxval(wgust)

! ----------------------------------------

! DEC is 3.0 in original code - should I change?
       DEC=3.0
!      DEC=6.0

       CALL FILL_FLD(GFLD,NUMV,IM,JM,DOWNP)

       GFLD%ipdtmpl(1)=3
       GFLD%ipdtmpl(2)=0
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0

       CALL set_scale(gfld, DEC)
       CALL PUTGB2(70,GFLD,IRET) ! DOWNP

         print *,'Output Downscaled Pressure',FHR
        write(0,*) 'IRET for DOWNP PUTGB2: ', IRET
         print *, 'DOWNP',minval(downp),maxval(downp)


!        Output high res topo,land for nests ??
!        Output topo for all grids 03-07-13
         print *,'Output High Res Topography',FHR

         DEC=-2.0

       CALL FILL_FLD(GFLD,NUMV,IM,JM,TOPO)

       GFLD%ipdtmpl(1)=3
       GFLD%ipdtmpl(2)=5
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0

       CALL set_scale(gfld, DEC)
       CALL PUTGB2(70,GFLD,IRET)  ! TOPO
         print *, 'TOPO',minval(topo),maxval(topo)

! ----------------------------------------
! Comment out CS2P - Expanded CONUS Nest reads in land cover now instead of Vegetation Type
!!       IF (REGION .NE. 'CS' .and. REGION .NE.'CS2P' )THEN
         IF (REGION .NE. 'CS')THEN

         DEC=1.0

       CALL FILL_FLD(GFLD,NUMV,IM,JM,VEG_NDFD)

       GFLD%discipline=2

       GFLD%ipdtmpl(1)=0
       GFLD%ipdtmpl(2)=0 ! was 198
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0

        write(0,*) 'min,max(VEG_NDFD): ', minval(VEG_NDFD), &
                                          maxval(VEG_NDFD)
           print *,'Output High Res Veg Fraction',FHR

       CALL set_scale(gfld, DEC)
       CALL PUTGB2(70,GFLD,IRET) ! VEG_NDFD
         print *, 'VEG_NDFD',minval(veg_ndfd),maxval(veg_ndfd)

         ENDIF

!  Boundary layer computations, find the # levels within the lowest 180 mb
!??? do we need to check for validpt ????
         print *, 'Calculate PBL Levels',FHR
         ktop=kmax
         if(lnest)ktop=35
         DO J=1,JM
         DO I=1,IM
           PBLMARK(I,J)=1
           TOP=PSFC(I,J)-18000.
           DO L=ktop,1,-1
             IF(PMID(I,J,L).GT.TOP)THEN
               PBLMARK(I,J)=L
               GOTO 60
             ENDIF 
           ENDDO
 60        CONTINUE
         ENDDO
         ENDDO

!  Compute RH
         print *, 'Calculate RH',FHR
         ALLOCATE (RH(IM,JM,KMAX),STAT=kret)
         ktop=kmax

        write(0,*) 'kmax, ktop: ', kmax, ktop

         DO J=1,JM
         DO I=1,IM
           if (validpt(I,J)) then
              DO L=1,ktop
                RH(I,J,L)=Q(I,J,L)/CalcQ(PMID(I,J,L),T(I,J,L))
              ENDDO
           endif
        ENDDO
        ENDDO

!  skip precip fields if FHR=0
        IF (FHR .EQ. 0) GOTO 444
! Skip POP fields for off-cycles of NAMRR
!       if (cyc .ne. 00 .and. cyc .ne. 06 .and. cyc .ne. 12 .and. cyc .ne.18)goto 444 
        if (HAVESREF .eq. 0)goto 444
!         CALL OUTPRCP
!--------------------------------------------------------------------------
! QPF - simply take model QPF and change units to inches
!---------------------------------------- --------------------------------
  
         print *, 'Calculate QPF',FHR
         ALLOCATE (QPF3(IM,JM),QPF6(IM,JM),QPF12(IM,JM),STAT=kret)
         QPF3  = P03M / 25.4   ! convert from millimeters to inches
         QPF6  = P06M / 25.4
         QPF12 = P12M / 25.4
!  with interpolations and lack of precision in the code dealing
!  with buckets, 3-hr totals may occasionally end up slightly
!  larger than 6 or 12-hr totals, and we don't want that

         WHERE (QPF3 .GT. QPF12 .AND.  QPF12 .NE. 0.0) QPF3=QPF12
         WHERE (QPF3 .GT. QPF6  .AND.  QPF6 .NE. 0.0)  QPF3=QPF6
!--------------------------------------------------------------------------
!     COMPUTE POPS
!--------------------------------------------------------------------------
         print *, 'Compute POPs',FHR
         ALLOCATE (POP3(IM,JM),POP6(IM,JM),POP12(IM,JM),STAT=kret)
         POP3=SPVAL;POP6=SPVAL;POP12=SPVAL

! 3-hr POP
         CALL MKPOP(PBLMARK,RH,BLI,P3CP01,P3CP10,P12CP01,P12CP10,QPF3,POP3,GDIN,3,VALIDPT)
         CALL BOUND(POP3,0.,100.)

                DEC=3.0

         IF (trim(CORE) .NE. 'GFS') THEN
           print *, 'Output 03 hr POP and precip',FHR

       CALL FILL_FLD(GFLD8,NUMV,IM,JM,POP3)

       GFLD8%idrtnum=40 ! 40 = JPEG
       GFLD8%idrtmpl(5)=0
       GFLD8%idrtmpl(6)=0
       GFLD8%idrtmpl(7)=-1

       GFLD8%discipline=1
       GFLD8%ipdtnum=8     ! should be superfluous

       GFLD8%ipdtmpl(1)=1
       GFLD8%ipdtmpl(2)=2
       GFLD8%ipdtmpl(9)=FHR3
       GFLD8%ipdtmpl(10)=1
       GFLD8%ipdtmpl(12)=0

       GFLD8%ipdtmpl(22)=1
       GFLD8%ipdtmpl(27)=3

       CALL set_scale(gfld8, DEC)
       CALL PUTGB2(70,GFLD8,IRET) ! POP3
         ENDIF

! ----------------------------------------
       CALL FILL_FLD(GFLD8,NUMV,IM,JM,P03M)

        write(0,*) 'maxval(P03M) at write: ', maxval(P03m)
        write(0,*) 'sum(P03M) at write: ', sum(P03M)
        write(0,*) 'maxval(gfld8%fld) : ', maxval(gfld8%fld)
        write(0,*) 'sum(gfld8%fld): ', sum(gfld8%fld)

       GFLD8%discipline=0
       GFLD8%ipdtnum=8

       GFLD8%ipdtmpl(1)=1
       GFLD8%ipdtmpl(2)=8
       GFLD8%ipdtmpl(9)=FHR3
       GFLD8%ipdtmpl(10)=1
       GFLD8%ipdtmpl(12)=0

       GFLD8%ipdtmpl(22)=1
       GFLD8%ipdtmpl(27)=3


       CALL set_scale(gfld8, DEC)
       CALL PUTGB2(70,GFLD8,IRET) ! P03M
 
! ----------------------------------------

! 6-hr POP
        IF(MOD(FHR,6).EQ.0) THEN
          CALL MKPOP(PBLMARK,RH,BLI,P6CP01,P6CP10,P12CP01,P12CP10,QPF6,POP6,GDIN,6,VALIDPT)
          WHERE(POP6.LT.POP3) POP6=POP3
          CALL BOUND(POP6,0.,100.)

          ID(1:25) = 0
          ID(8)=193;ID(9)=1
          ID(18)=FHR6;ID(19)=FHR
          ID(20)=4
          DEC=3.0
          IF (trim(CORE) .NE. 'GFS') THEN
            print *, 'Output 06 hr POP and precip',FHR

         DEC=3.0

       CALL FILL_FLD(GFLD8,NUMV,IM,JM,POP6)

       GFLD8%discipline=1
       GFLD8%ipdtnum=8     ! should be superfluous

       GFLD8%ipdtmpl(1)=1
       GFLD8%ipdtmpl(2)=2
       GFLD8%ipdtmpl(9)=FHR6
       GFLD8%ipdtmpl(10)=1
       GFLD8%ipdtmpl(12)=0

       GFLD8%ipdtmpl(22)=1
       GFLD8%ipdtmpl(27)=6

       CALL set_scale(gfld8, DEC)
       CALL PUTGB2(70,GFLD8,IRET) ! POP6

          ENDIF

! Test output SREF PoP > .01"
!          ID(8)=194;ID(9)=1
!          CALL GRIBIT(ID,RITEHD,P6CP01,GDIN,70,DEC)

! ----------------------------------------------------

       CALL FILL_FLD(GFLD8,NUMV,IM,JM,P06M)

       GFLD8%discipline=0
       GFLD8%ipdtnum=8     ! should be superfluous

       GFLD8%ipdtmpl(1)=1
       GFLD8%ipdtmpl(2)=8
       GFLD8%ipdtmpl(9)=FHR6
       GFLD8%ipdtmpl(10)=1
       GFLD8%ipdtmpl(12)=0

       GFLD8%ipdtmpl(22)=1
       GFLD8%ipdtmpl(27)=6

       CALL set_scale(gfld8, DEC)
       CALL PUTGB2(70,GFLD8,IRET) ! P06M
         
        ENDIF

! 12-hr POP
       IF (LHR12) THEN
         IF(LCYCON .OR. .NOT.LCYCON.AND.FHR.NE.6) THEN
           CALL MKPOP(PBLMARK,RH,BLI,P12CP01,P12CP10,P12CP01,P12CP10,QPF12,POP12,GDIN,12,VALIDPT)
           WHERE (POP12.LT.POP6) POP12=POP6
           CALL BOUND(POP12,0.,100.)

           IF (trim(CORE) .NE. 'GFS') THEN
             print *, 'Output 12 hr precip',FHR

         DEC=3.0

       CALL FILL_FLD(GFLD8,NUMV,IM,JM,POP12)

       GFLD8%discipline=1
       GFLD8%ipdtnum=8     ! should be superfluous

       GFLD8%ipdtmpl(1)=1
       GFLD8%ipdtmpl(2)=2
       GFLD8%ipdtmpl(9)=FHR12
       GFLD8%ipdtmpl(10)=1
       GFLD8%ipdtmpl(12)=0

       GFLD8%ipdtmpl(22)=1
       GFLD8%ipdtmpl(27)=12

       CALL set_scale(gfld8, DEC)
       CALL PUTGB2(70,GFLD8,IRET) ! POP12

           ENDIF

       CALL FILL_FLD(GFLD8,NUMV,IM,JM,P12M)

       GFLD8%discipline=0
       GFLD8%ipdtnum=8     ! should be superfluous

       GFLD8%ipdtmpl(1)=1
       GFLD8%ipdtmpl(2)=8
       GFLD8%ipdtmpl(9)=FHR12
       GFLD8%ipdtmpl(10)=1
       GFLD8%ipdtmpl(12)=0

       GFLD8%ipdtmpl(22)=1
       GFLD8%ipdtmpl(27)=12

       CALL set_scale(gfld8, DEC)
       CALL PUTGB2(70,GFLD8,IRET) ! P12M

         ENDIF
      ENDIF

      print *, 'Compute GRIDWX',FHR
      ALLOCATE (WXSTRING(IM,JM),GRIDWX(IM,JM),STAT=kret)
      CALL MAKESTRING(IRAIN,ISNOW,IZR,IIP,BLI,POP3,GDIN,WXSTRING,GRIDWX,VALIDPT)

      DEC=3.0

       CALL FILL_FLD(GFLD,NUMV,IM,JM,GRIDWX)

       GFLD%discipline=0
       GFLD%ipdtnum=0
       GFLD%ipdtmpl(1)=1
       GFLD%ipdtmpl(2)=192
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0

       CALL set_scale(gfld, DEC)
       CALL PUTGB2(70,GFLD,IRET) ! GRIDWX

!    #--------------------------------------------------------------------------
!    #  Chance of Wetting Rain (0.1 inch).  Same algorithm as PoP, but requires
!    #       more model QPF to get same chances, and higher boundary layer RH
!    #       to get the adjustment (and maximum adjustment is less).
!    #--------------------------------------------------------------------------
       IF (trim(CORE) .NE. 'GFS') THEN
         print *, 'Compute WETTING RAIN',FHR
         QPFMAX=0.60    ! QPF value where raw PoP would be 75%
         RHexcess=70.0  ! RH above this can add to PoP and below will subtract
         adjAmount=15.0 ! amount of adjustment allowed
         DO I = 1, IM
         DO J = 1, JM
           if(validpt(i,j)) then
             lmbl=int(pblmark(i,j))
!JTM     Corrected  ERROR...Added *100 to compute rhavg 11/25/12
             rhavg=100*SUM(rh(i,j,1:lmbl))/lmbl
             tmpcwr=calcw(qpf3(i,j),lmbl,rhavg,qpfmax,RHexcess,adjAmount)
             CWR(I,J)=(TMPCWR+2*P3CP10(I,J))/3.
           endif
         ENDDO
         ENDDO
!????   nests and HI uses 25 for max limit ???       
         WHERE (validpt)
           WHERE (QPF3.GT. 0.10) CWR=AMAX1(CWR,25.)
           WHERE (CWR.GT.POP3) CWR=POP3
         ENDWHERE
         CALL BOUND(CWR,0.,100.)

                 DEC=3.0

       CALL FILL_FLD(GFLD8,NUMV,IM,JM,CWR)

       GFLD8%discipline=1
       GFLD8%ipdtnum=8     ! should be superfluous

       GFLD8%ipdtmpl(1)=1
       GFLD8%ipdtmpl(2)=195
       GFLD8%ipdtmpl(9)=FHR3
       GFLD8%ipdtmpl(10)=1
       GFLD8%ipdtmpl(12)=0

       GFLD8%ipdtmpl(22)=1
       GFLD8%ipdtmpl(27)=3

       CALL set_scale(gfld8, DEC)
       CALL PUTGB2(70,GFLD8,IRET) ! CWR

       ENDIF !if core not equal gfs
!======================================================================
!--->   COMPUTE SNOWFALL  FOR 3 and 6 HR PERIODS
!======================================================================
       IF (trim(CORE) .NE. 'GFS') THEN
         print *, 'Compute SNOWFALL',FHR
         ALLOCATE (SNOWAMT3(IM,JM),SNOWAMT6(IM,JM),STAT=kret)

! HI snowamt3 uses temp < 264 check ????
         SNOWAMT3=SPVAL
         CALL SNOWFALL(SN03,SNOWAMT3,DOWNT,THOLD,GDIN,3.,VALIDPT)

                 DEC=3.0

       CALL FILL_FLD(GFLD8,NUMV,IM,JM,SNOWAMT3)

       GFLD8%discipline=0
       GFLD8%ipdtnum=8     ! should be superfluous

       GFLD8%ipdtmpl(1)=1
       GFLD8%ipdtmpl(2)=11
       GFLD8%ipdtmpl(9)=FHR3
       GFLD8%ipdtmpl(10)=1
       GFLD8%ipdtmpl(12)=0

       GFLD8%ipdtmpl(22)=1
       GFLD8%ipdtmpl(27)=3

       CALL set_scale(gfld8, DEC)
       CALL PUTGB2(70,GFLD8,IRET) ! SNOWAMT3

         do isn=1,im
         do jsn=1,jm
           if (snowamt3(isn,jsn).lt.-0.2) then
             print *, 'i,j,SN0,T',isn,jsn,SN03(isn,jsn),T(isn,jsn,1),validpt(isn,jsn)
             print *, 'DOWNT,THOLD',DOWNT(isn,jsn),THOLD(isn,jsn,3),THOLD(isn,jsn,2),SNOWAMT3(isn,jsn)
           endif
         enddo
         enddo

         IF (MOD(FHR,6).EQ.0)  THEN  
           SNOWAMT6=SPVAL
        write(0,*) 'maxval(SN06): ', maxval(SN06)
           CALL SNOWFALL(SN06,SNOWAMT6,DOWNT,THOLD,GDIN,4.,VALIDPT)

                  CALL FILL_FLD(GFLD8,NUMV,IM,JM,SNOWAMT6)

       GFLD8%discipline=0
       GFLD8%ipdtnum=8     ! should be superfluous

       GFLD8%ipdtmpl(1)=1
       GFLD8%ipdtmpl(2)=11
       GFLD8%ipdtmpl(9)=FHR6
       GFLD8%ipdtmpl(10)=1
       GFLD8%ipdtmpl(12)=0

       GFLD8%ipdtmpl(22)=1
       GFLD8%ipdtmpl(27)=6

       CALL set_scale(gfld8, DEC)
       CALL PUTGB2(70,GFLD8,IRET) ! SNOWAMT6

         ENDIF
       ENDIF !if core not equal gfs

 444    CONTINUE

!======================================================================
!--->   COMPUTE SKY COVER
!======================================================================
        print *, 'Compute SKYCVR',FHR
        ALLOCATE (TEMP1(IM,JM),TEMP2(IM,JM),STAT=kret)
        ALLOCATE (SKY(IM,JM),STAT=kret)
         if(lnest.and. .not.lhiresw) then
           SKY=SPVAL
           where(validpt)
             TEMP1=AMAX1(LCLD,MCLD)
             SKY=AMAX1(TEMP1,HCLD)
           endwhere
         else
          CALL SKYCVR(SKY,CFR,GDIN)
          CALL BOUND (SKY,0.,100.)
        endif
        IF(trim(CORE) .EQ. 'GFS' ) THEN
          print *, 'Computing skycvr for GFS DNG'
          CALL SKYCVR(SKY,CFR,GDIN)
          CALL BOUND (SKY,0.,100.)
        ENDIF
        DEALLOCATE (TEMP1,TEMP2,STAT=kret)

        IF (MOD(FHR,3).EQ.0 .AND. trim(CORE) .EQ. 'GFS' ) THEN
                DEC=3.0

        CALL FILL_FLD(GFLD,NUMV,IM,JM,SKY)

       GFLD%discipline=0
       GFLD%ipdtnum=0
       GFLD%ipdtmpl(1)=6
       GFLD%ipdtmpl(2)=1
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0

        CALL set_scale(gfld, DEC)
        CALL PUTGB2(70,GFLD,IRET) ! SKY

        ELSEIF (trim(CORE) .NE. 'GFS' ) THEN

                DEC=3.0

        CALL FILL_FLD(GFLD,NUMV,IM,JM,SKY)

       GFLD%discipline=0
       GFLD%ipdtnum=0
       GFLD%ipdtmpl(1)=6
       GFLD%ipdtmpl(2)=1
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0

        CALL set_scale(gfld, DEC)
        CALL PUTGB2(70,GFLD,IRET) ! SKY

        ENDIF

        IF (trim(CORE) .NE. 'GFS' ) THEN
          print *, 'Output Reflectivity',FHR


        DEC=3.0

        print*, 'return with REFC min/max: ', minval(REFC),maxval(REFC)
       CALL FILL_FLD(GFLD,NUMV,IM,JM,REFC)

       GFLD%discipline=0
       GFLD%ipdtnum=0
       GFLD%ipdtmpl(1)=16
       GFLD%ipdtmpl(2)=196 ! old version more familiar to downstream codes?
       GFLD%ipdtmpl(10)=200
       GFLD%ipdtmpl(12)=0

       CALL set_scale(gfld, DEC)
       CALL PUTGB2(70,GFLD,IRET) ! REFC

        ENDIF

!========================================================================
! calcSnowLevel - takes sounding of the wetbulb temperature and finds the
!   lowest elevation (above ground) where wetbulb crosses from
!   above freezing to below freezing. When top wetbulb is above
!   freezing - puts in height of top level.   We now use this
!   field straight out of the NAM. 
!========================================================================

      print *, 'Output Snow Level',FHR
      DEC=3.0

       CALL FILL_FLD(GFLD,NUMV,IM,JM,WETFRZ)

       GFLD%discipline=0
       GFLD%ipdtnum=0
       GFLD%ipdtmpl(1)=3
       GFLD%ipdtmpl(2)=5
       GFLD%ipdtmpl(10)=245
       GFLD%ipdtmpl(12)=0

       CALL set_scale(gfld, DEC)
       CALL PUTGB2(70,GFLD,IRET) ! WETFRZ
       write(0,*) 'IRET for WETFRZ: ', IRET

! VISIBILITY
      print *, 'Output Visibility',FHR

      DEC=2.7

       CALL FILL_FLD(GFLD,NUMV,IM,JM,VIS)

       GFLD%discipline=0
       GFLD%ipdtnum=0
       GFLD%ipdtmpl(1)=19
       GFLD%ipdtmpl(2)=0
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0

       CALL set_scale(gfld, DEC)
       CALL PUTGB2(70,GFLD,IRET)  ! VIS
        write(0,*) 'IRET for VIS: ', IRET


! CLOUD CEILING HEIGHT
      IF (trim(CORE) .NE. 'dgx' ) THEN
      print*, 'Output Cloud Ceiling Height', FHR
      DEC=-5.0

       CALL FILL_FLD(GFLD,NUMV,IM,JM,CEIL)

       GFLD%discipline=0
       GFLD%ipdtnum=0
       GFLD%ipdtmpl(1)=3
       GFLD%ipdtmpl(2)=5
       GFLD%ipdtmpl(10)=215
       GFLD%ipdtmpl(12)=0

       CALL set_scale(gfld, DEC)
       CALL PUTGB2(70,GFLD,IRET)  ! CEIL
        write(0,*) 'IRET for CEIL: ', IRET

      print*,'maxval(CEIL),minval(CEIL): ', maxval(CEIL),minval(CEIL)

! SLP
      print*, 'Output SLP', FHR
! MSLET (Mesinger/Membrane)
      DEC=-0.1

       CALL FILL_FLD(GFLD,NUMV,IM,JM,SLP)

       GFLD%discipline=0
       GFLD%ipdtnum=0
       GFLD%ipdtmpl(1)=3
       GFLD%ipdtmpl(2)=192
       GFLD%ipdtmpl(10)=101
       GFLD%ipdtmpl(12)=0

       CALL set_scale(gfld, DEC)
       CALL PUTGB2(70,GFLD,IRET)  ! SLP
        write(0,*) 'IRET for SLP: ', IRET
        write(0,*) 'maxval(SLP),minval(SLP): ', maxval(SLP),minval(SLP)
      print*,'maxval(SLP),minval(SLP): ', maxval(SLP),minval(SLP)

! SST - this is really Skin T/SST, but we are writing it out as 2-m Temperature,
! since Skin T/SST is already being used (per Geoff DiMego).
! Disable for this implementation - revisit for next implementation
!     print*, 'Output SST', FHR
!     DEC=-2.0

!      CALL FILL_FLD(GFLD,NUMV,IM,JM,SST)

!      GFLD%discipline=0
!      GFLD%ipdtnum=0
!      GFLD%ipdtmpl(1)=0
!      GFLD%ipdtmpl(2)=0
!      GFLD%ipdtmpl(10)=103
!      GFLD%ipdtmpl(12)=2

!      CALL set_scale(gfld, DEC)
!      CALL PUTGB2(70,GFLD,IRET)  ! SST
        write(0,*) 'IRET for SST: ', IRET
        write(0,*) 'maxval(SST),minval(SST): ', maxval(SST),minval(SST)
      print*,'maxval(SST),minval(SST): ', maxval(SST),minval(SST)

!     ID(1:25) = 0
!     ID(8)=11;ID(9)=105
!     ID(11)=2
!     CALL GRIBIT(ID,RITEHD,SST,GDIN,70,DEC)
!     print*,'maxval(SST),minval(SST): ', maxval(SST),minval(SST)

      endif ! dgx

!==========================================================================
!  TransWind - the average winds in the layer between the surface
!              and the mixing height.
!--------------------------------------------------------------------------

      print *, 'Compute TransWind',FHR
      ALLOCATE (MGTRANS(IM,JM),DIRTRANS(IM,JM),STAT=kret)
      MGTRANS=SPVAL;DIRTRANS=SPVAL
      DO J=1,JM
       DO I=1,IM
        if(validpt(i,j)) then
        MGD=SQRT(DOWNU(I,J)*DOWNU(I,J)+DOWNV(I,J)*DOWNV(I,J)) 
        UTOT=0.
        VTOT=0.
        LMBL=INT(PBLMARK(I,J))
         UTOT=SUM(UWND(I,J,1:LMBL))
         VTOT=SUM(VWND(I,J,1:LMBL))
        UTRANS=UTOT/LMBL
        VTRANS=VTOT/LMBL
        MGTRANS(I,J)=SQRT(UTRANS*UTRANS+VTRANS*VTRANS) 
        IF (MGTRANS(I,J).EQ.0.) THEN
         DIRTRANS(I,J)=0.
        ELSE
         DIRTRANS(I,J)=ATAN2(-UTRANS,-VTRANS) / 0.0174
        ENDIF
        IF(DIRTRANS(I,J).LT.0.) DIRTRANS(I,J)=DIRTRANS(I,J)+360.0
        IF(DIRTRANS(I,J).GT.360.) DIRTRANS(I,J)=DIRTRANS(I,J)-360.0
        endif
       ENDDO
      ENDDO

      DEC=3.0
      print *, 'Output PBL Wind Direction',FHR

       CALL FILL_FLD(GFLD,NUMV,IM,JM,DIRTRANS)

       GFLD%discipline=0
       GFLD%ipdtnum=0
       GFLD%ipdtmpl(1)=2
       GFLD%ipdtmpl(2)=0
       GFLD%ipdtmpl(10)=220
       GFLD%ipdtmpl(12)=0

       CALL set_scale(gfld, DEC)
       CALL PUTGB2(70,GFLD,IRET) ! DIRTRANS
        write(0,*) 'IRET for PBL WINDIR: ', IRET

      DEC=-3.0
      print *, 'Output PBL Wind Speed',FHR


       CALL FILL_FLD(GFLD,NUMV,IM,JM,MGTRANS)

       GFLD%discipline=0
       GFLD%ipdtnum=0
       GFLD%ipdtmpl(1)=2
       GFLD%ipdtmpl(2)=1
       GFLD%ipdtmpl(10)=220
       GFLD%ipdtmpl(12)=0

       CALL set_scale(gfld, DEC)
       CALL PUTGB2(70,GFLD,IRET) ! MGTRANS
        write(0,*) 'IRET for PBL WINSPD: ', IRET

!  compute PBL RH

      print *, 'Compute PBL RH',FHR
      ALLOCATE (BLR(IM,JM),STAT=kret)
      BLR=SPVAL
      DO J=1,JM
      DO I=1,IM
        if(validpt(i,j)) then
        BLH=INT(PBLMARK(I,J))
        RHSUM=SUM(RH(I,J,1:BLH))
        LEVS=BLH+1
        BLR(I,J)=(RHSUM/LEVS)*100.
        endif
      ENDDO
      ENDDO
      CALL BOUND(BLR,0.,100.)

      ID(1:25) = 0
      ID(8)=52;ID(9)=220
      DEC=3.0


       CALL FILL_FLD(GFLD,NUMV,IM,JM,BLR)

       GFLD%discipline=0
       GFLD%ipdtnum=0
       GFLD%ipdtmpl(1)=1
       GFLD%ipdtmpl(2)=1
       GFLD%ipdtmpl(10)=220
       GFLD%ipdtmpl(12)=0

       CALL set_scale(gfld, DEC)
       CALL PUTGB2(70,GFLD,IRET) ! BLR
        write(0,*) 'IRET for PBL RH: ', IRET

!========================================================================
!  MixHgt - the height to which a parcel above a 'fire' would rise
!    (in height) above ground level (in feet).

!  Calculated by assuming a parcel above a fire is VERY hot - but the fire
!  is very small - so that entrainment quickly makes it only a few degrees
!  warmer than the environment.  Ideally would want to consider moisture
!  and entrainment - but this is a very simple first guess.
!========================================================================

      print *, 'Compute MIXHGT',FHR
      ALLOCATE (MIXHGT(IM,JM),STAT=kret)
      MIXHGT=SPVAL
      ktop=kmax
      if(lnest)ktop=40   
      DO J=1,JM
      DO I=1,IM
       if(validpt(i,j))then
       firetheta=((P1000/PSFC(I,J))**CAPA)*(T2(I,J)+2.0)
       DO L=2,ktop
         theta=((P1000/PMID(I,J,L))**CAPA)*(T(I,J,L))
         IF (theta.gt.firetheta) THEN
           MIXHGT(I,J)=HGHT(I,J,L)-ZSFC(I,J)
           GOTO 321
         ENDIF
       ENDDO
       MIXHGT(I,J)=HGHT(I,J,ktop)+300.   ! 07/13: Fix to ensure mixhgt definition
       endif
 321  continue
      ENDDO
      ENDDO

      DEC=-3.0    ! HI = +3.0 ?????


       CALL FILL_FLD(GFLD,NUMV,IM,JM,MIXHGT)

       GFLD%discipline=0
       GFLD%ipdtnum=0
!       GFLD%ipdtmpl(1)=19
!       GFLD%ipdtmpl(2)=3
       GFLD%ipdtmpl(1)=3
       GFLD%ipdtmpl(2)=6
       GFLD%ipdtmpl(10)=220
       GFLD%ipdtmpl(12)=0


       CALL set_scale(gfld, DEC)
       CALL PUTGB2(70,GFLD,IRET) ! MIXHGT
        write(0,*) 'IRET for PBL MIXHGT: ', IRET

!--------------------------------------------------------------------------
! LAL - Based mainly on lifted index.  Adds more when RH at top of BL is
!       high, but RH at bottom of BL is low.
!--------------------------------------------------------------------------

      print *, 'Compute LAL',FHR
      ALLOCATE (LAL(IM,JM),STAT=kret)
      LAL=SPVAL
      DO J=1,JM
      DO I=1,IM

      if(validpt(i,j)) then
       IF (BLI(I,J).LT.-5.) THEN;     LLAL=4.
       ELSE IF (BLI(I,J).LT.-3) THEN; LLAL=3.
       ELSE IF (BLI(I,J).LT.0)  THEN; LLAL=2.
       ELSE;LLAL=1.
       END IF 
!   Add more when RH at top of BL is greater than
!      than 70% and RH at bottom of BL is less than 30

!----------------Make into subroutine lal
       RH1TOT=0.; RH1SUM=0.; RH2TOT=0.;  RH2SUM=0.
       DO L=1,40
        IF(PSFC(I,J)-PMID(I,J,L).LT.3000.) THEN
          RH1TOT=RH1TOT+RH(I,J,L)
          RH1SUM=RH1SUM+1.
        ENDIF
        IF(PSFC(I,J)-PMID(I,J,L).LT.18000. .AND.  &
          PSFC(I,J)-PMID(I,J,L).GT.15000.) THEN
          RH2TOT=RH2TOT+RH(I,J,L)
          RH2SUM=RH2SUM+1.
        ENDIF
       ENDDO
!      RH1=RH1TOT/RH1SUM
!      RH2=RH2TOT/RH2SUM 
! Should I keep?  Matt has in his code.

        if (RH1SUM .ge. 1) then
       RH1=RH1TOT/RH1SUM
        else
        write(0,*) 'RH1SUM is zero...would divide by zero'
       RH1=0.2
        endif

        if (RH2SUM .ge. 1) then
       RH2=RH2TOT/RH2SUM
        else
        write(0,*) 'RH2SUM is zero...would divide by zero'
       RH2=0.8
        endif

       IF (RH2.GT.0.8 .AND. RH1.LT.0.2) THEN
        LAL(I,J)=LLAL+1.
       ELSE
        LAL(I,J)=LLAL
       ENDIF
       IF (LAL(I,J) .LT.-18.) LAL(I,J)=1.
      endif
      ENDDO
      ENDDO


            DEC=2.0     ! HI DEC=3.0 ????
        print*, 'past LAL write'

       CALL FILL_FLD(GFLD,NUMV,IM,JM,LAL)

       GFLD%discipline=0
       GFLD%ipdtnum=0
       GFLD%ipdtmpl(1)=7
       GFLD%ipdtmpl(2)=193 ! or 195 - CWDI?
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0

       CALL set_scale(gfld, DEC)
       CALL PUTGB2(70,GFLD,IRET) ! LAL
        write(0,*) 'IRET for LAL: ', IRET

!     Compute Haines Index
      IF (trim(CORE) .NE. 'GFS' ) THEN
        ALLOCATE (TEMP1(IM,JM),TEMP2(IM,JM),STAT=kret)
        print *,'Compute HAINES INDEX'
        CALL HINDEX(IM,JM,HAINES,HLVL,VALIDPT)
        print*, 'return with min/max: ', minval(HAINES),maxval(HAINES)

! Should I change to 3.0 for DEC?
        DEC=1.0 ! original value
!     DEC=3.0
        TEMP1=real(HAINES)
! Set special values for undefined areas of domain
        where (.not. validpt)
          TEMP1=SPVAL 
        endwhere 
        print*, 'return with min/max: ', minval(TEMP1),maxval(TEMP1)
       CALL FILL_FLD(GFLD,NUMV,IM,JM,TEMP1)

       GFLD%discipline=2
       GFLD%ipdtnum=0
       GFLD%ipdtmpl(1)=4
       GFLD%ipdtmpl(2)=2
       GFLD%ipdtmpl(9)=gdin%FHR
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0

       CALL set_scale(gfld, DEC)
       CALL PUTGB2(70,GFLD,IRET) ! HINDEX

!       CALL GRIBIT(ID,RITEHD,TEMP1,GDIN,70,DEC)

!        ID(2)=2
!        ID(8)=209;ID(9)=1
!        DEC=1.0
!        TEMP2=real(HLVL)
!        CALL GRIBIT(ID,RITEHD,TEMP2,GDIN,70,DEC)
        DEALLOCATE (TEMP1,TEMP2,STAT=kret)
      ENDIF

!=================================================
    ENDIF  ! END 3 hour writes
!=================================================

    IF(LCYCON .AND. .NOT.LHR12 .OR.             &
      .NOT.LCYCON.AND.MOD(FHR-6,12).NE.0) THEN 
      print *, 'going to write minmax ', fhr
      RITEHD = .TRUE.

!      FNAME2OUT='MAXMING2.fxx'
       FNAME='MAXMIN'
       RESTHR='tm00'
       CDOT='.'
!      WRITE(DESCR2,'(I2.2)') FHR
!      IF (FHR.GE.100) WRITE(DESCR2,'(I3.3)') FHR
!      WRITE(FNAME2OUT(11:12),FMT='(I2.2)')FHR
       WRITE(DESCR2,'(I2.2)') FHR
       IF (FHR.GE.100) WRITE(DESCR2,'(I3.3)') FHR
        write(0,*) 'MAXMIN FHR known as: ', FHR
!      WRITE(FNAME(7:8),FMT='(I2.2)')FHR
!      IF (FHR.GE.100) WRITE(FNAME(7:9),FMT='(I3.3)')FHR
       FNAME2OUT = TRIM(FNAME)//TRIM(DESCR2)//CDOT//RESTHR
       write(0,*) 'FNAME2OUT(1:12): ', TRIM(FNAME2OUT)
       CALL BAOPEN(71,FNAME2OUT,IRET)
       write(0,*) 'IRET from BAOPEN of 71: ', IRET

        NUMV=IM*JM

       DEC=-2.0

        write(0,*) 'min,max DOWNT: ', minval(DOWNT),maxval(DOWNT)

       CALL FILL_FLD(GFLD,NUMV,IM,JM,DOWNT)

       GFLD%discipline=0
       GFLD%ipdtnum=0
       GFLD%ipdtmpl(1)=0
       GFLD%ipdtmpl(2)=0
!      GFLD%ipdtmpl(10)=103
!      GFLD%ipdtmpl(12)=2
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0

       CALL set_scale(gfld, DEC)
       CALL PUTGB2(71,GFLD,IRET) ! DOWNT for MAXMIN file


      DEC=-2.0

       CALL FILL_FLD(GFLD,NUMV,IM,JM,DOWNDEW)

       GFLD%discipline=0
       GFLD%ipdtnum=0
       GFLD%ipdtmpl(1)=0
       GFLD%ipdtmpl(2)=6
!      GFLD%ipdtmpl(10)=103
!      GFLD%ipdtmpl(12)=2
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0

       CALL set_scale(gfld, DEC)
       CALL PUTGB2(71,GFLD,IRET) ! DOWNDEW for MAXMIN file
        call baclose(71,iret)

    ENDIF

!   For Nests Write limited data to grib file for hrs 1,2,4,5,7,8,10,11
!   since these files serve as 1st guess for RTMA
!   The length of hourly file writes set by fhrhrly here and in nam_sminit.sh
!   11/2013: Conus 2.5 km hrly output extended to 36 hours for wave model input
      fhrhrly=12  
      IF (TRIM(CORE) .EQ. 'GFS' .AND. .NOT.LCYCON) fhrhrly=18
      IF (TRIM(REGION).EQ.'CS2P') fhrhrly=36
      print *, 'REGION ',TRIM(REGION),fhrhrly
      IF (TRIM(REGION).EQ.'HI' .or. TRIM(REGION).EQ.'PR'  & 
      .or. TRIM(REGION).EQ.'AK' .or. TRIM(REGION).EQ.'AK3' & 
      .or. TRIM(REGION).EQ.'AKRT' .or. TRIM(REGION).EQ.'CS2P' &
      .or. TRIM(REGION).EQ.'GM') THEN
         print *, 'GET GRIBLIMITIED ',TRIM(REGION),fhrhrly
!        IF(.not.LHR3 .AND. FHR.LT.fhrhrly) CALL GRIBLIMITED(SKY,70,GDIN)
         IF(.not.LHR3 .AND. FHR.LT.fhrhrly)then
           print *, 'Compute SKYCVR for limited files',FHR
           ALLOCATE (TEMP1(IM,JM),TEMP2(IM,JM),STAT=kret)
           ALLOCATE (SKY(IM,JM),STAT=kret)
             if(lnest.and. .not.lhiresw) then
               print*,'Compute SKYCVR using low, middle and high clouds from NAM Nest'
               SKY=SPVAL
               where(validpt)
                 TEMP1=AMAX1(LCLD,MCLD)
                 SKY=AMAX1(TEMP1,HCLD)
               endwhere
             else
               print*,'Compute SKYCVR from subroutine SKYCVR'
               CALL SKYCVR(SKY,CFR,GDIN)
               CALL BOUND (SKY,0.,100.)
             endif
             IF(trim(CORE) .EQ. 'GFS' ) THEN
               print *, 'Computing skycvr for GFS DNG'
               CALL SKYCVR(SKY,CFR,GDIN)
               CALL BOUND (SKY,0.,100.)
             ENDIF
             DEALLOCATE (TEMP1,TEMP2,STAT=kret)
           iunit=70
!          FNAMEOUT='smartg2.xx'
           FNAME='MESO'//TRIM(REGION)
           RESTHR='tm00'
           CDOT='.'
           WRITE(DESCR2,'(I2.2)') FHR
           IF (FHR.GE.100) WRITE(DESCR2,'(I3.3)') FHR

           write(0,*) 'FHR known as: ', FHR
!          WRITE(FNAMEOUT(9:10),FMT='(I2.2)')FHR
           FNAMEOUT = TRIM(FNAME)//TRIM(DESCR2)//CDOT//RESTHR
!          write(0,*) 'FNAMEOUT(1:10): ', FNAMEOUT(1:10)
           write(0,*) 'FNAMEOUT: ', TRIM(FNAMEOUT)

           CALL BAOPEN(70,FNAMEOUT,IRET)
           write(0,*) 'IRET from BAOPEN of 70: ', IRET

           CALL GRIBLIMITED(sky,iunit,GDIN,GFLD,IM,JM)
           call baclose(70,iret)
!            CALL GRIBLIMITED(SKY,70,GDIN)
           endif ! not.LHR3 .and. FHR .lt. fhrhrly
      ENDIF

!  write older T/Td data for max/min to grib file
      ALLOCATE (TEMP1(IM,JM),TEMP2(IM,JM),STAT=kret)
      IF (trim(GDIN%CORE) .EQ. 'GFS' .AND. .NOT. LCYCON .AND. LHR6 ) GOTO 581
      IF (trim(GDIN%CORE) .EQ. 'GFS' .AND. .NOT. LCYCON .AND. FHR .EQ. 15) GOTO 581
      IF (FHR.NE.0. .AND. LHR3) THEN
!        IF (.NOT.LHR12) THEN   ! output T at all hours
          DO ivarb=1,2
            ID(1:25) = 0
            DEC=-2.0
            TEMP1=SPVAL;TEMP2=SPVAL
            IF(ivarb.eq.1) then
              GFLD%discipline=0
              GFLD%ipdtnum=0
              GFLD%ipdtmpl(1)=0
              GFLD%ipdtmpl(2)=0
!             GFLD%ipdtmpl(10)=103
!             GFLD%ipdtmpl(12)=2
              GFLD%ipdtmpl(10)=1
              GFLD%ipdtmpl(12)=0
              where (VALIDPT) 
                TEMP1=THOLD(:,:,3)   ! 1 hour old Temp
                TEMP2=THOLD(:,:,2)   ! 2 hour old Temp
! JTM 01-27-2013
! Added check for single points when temperature=0 at validpt 
! This should not happen but has been found on some nests
!                where (temp1.le.10) TEMP1=SPVAL
!                where (temp2.le.10) TEMP2=SPVAL
              end where
            else
              GFLD%discipline=0
              GFLD%ipdtnum=0
              GFLD%ipdtmpl(1)=0
!             GFLD%ipdtmpl(2)=6
!             GFLD%ipdtmpl(10)=103
              GFLD%ipdtmpl(2)=6
              GFLD%ipdtmpl(10)=1
              GFLD%ipdtmpl(12)=0
              where (VALIDPT) 
                TEMP1=DHOLD(:,:,3)
                TEMP2=DHOLD(:,:,2)
!               where (temp1.le.10) TEMP1=SPVAL
!               where (temp2.le.10) TEMP2=SPVAL
              end where
            endif ! ivarb.eq. 1

            GDIN%FHR=GDIN%FHR-1  ! change current hr to prev. hr for GRIBIT 
            IF ( trim(GDIN%CORE) .EQ. 'GFS' .AND. GDIN%FHR .LE. FHRHRLY ) THEN
              print *,'GFS OUTPUT MAX-MIN for FHR',GDIN%FHR
!             CALL GRIBIT(ID,RITEHD,TEMP1,GDIN,70,DEC)
                  CALL FILL_FLD(GFLD,NUMV,IM,JM,TEMP1)

!      GFLD%ipdtmpl(9)=gdin%FHR-1
       GFLD%ipdtmpl(9)=gdin%FHR ! 1 is already subtracted above

       CALL set_scale(gfld, DEC)
       CALL PUTGB2(70,GFLD,IRET) ! TEMP or TD (-1 h)

              print *,'GFS OUTPUT MAX-MIN for FHR',GDIN%FHR


              GDIN%FHR=GDIN%FHR-1  ! change current hr to FHR-2
              print *,'GFS OUTPUT MAX-MIN for FHR',GDIN%FHR
!             CALL GRIBIT(ID,RITEHD,TEMP2,GDIN,70,DEC)

       CALL FILL_FLD(GFLD,NUMV,IM,JM,TEMP2)
!      GFLD%ipdtmpl(9)=gdin%FHR-2
       GFLD%ipdtmpl(9)=gdin%FHR-1 ! 1 is already subtracted above
       CALL set_scale(gfld, DEC)
       CALL PUTGB2(70,GFLD,IRET) ! TEMP or TD (-2 h)

            ELSEIF ( trim(GDIN%CORE) .NE. 'GFS' ) THEN
              print *,'OUTPUT Temperature for FHR',GDIN%FHR
                  CALL FILL_FLD(GFLD,NUMV,IM,JM,TEMP1)

!      GFLD%ipdtmpl(9)=gdin%FHR-1
       GFLD%ipdtmpl(9)=gdin%FHR ! 1 is already subtracted above

       CALL set_scale(gfld, DEC)
       CALL PUTGB2(70,GFLD,IRET) ! TEMP or TD (-1 h)

!             CALL GRIBIT(ID,RITEHD,TEMP1,GDIN,70,DEC)
!             GDIN%FHR=GDIN%FHR-1  ! change current hr to FHR-2
       CALL FILL_FLD(GFLD,NUMV,IM,JM,TEMP2)
!      GFLD%ipdtmpl(9)=gdin%FHR-2
       GFLD%ipdtmpl(9)=gdin%FHR-1 ! 1 is already subtracted above
       CALL set_scale(gfld, DEC)
       CALL PUTGB2(70,GFLD,IRET) ! TEMP or TD (-2 h)

              print *,'OUTPUT Temperature for FHR',GDIN%FHR
!             CALL GRIBIT(ID,RITEHD,TEMP2,GDIN,70,DEC)
            ENDIF
            GDIN%FHR=IFHRIN;FHR=IFHRIN;IFHR=IFHRIN
          ENDDO 
!        ENDIF
      ENDIF
        write(6,*) 'past write of older T'
      DEALLOCATE (TEMP1,TEMP2,STAT=kret)
        write(6,*) 'past dealloc of TEMP1, TEMP2'
 581 CONTINUE

!  compute max/min temps for 3,6,9,12.....
     ALLOCATE(TMAX3(IM,JM),RHMAX3(IM,JM),STAT=kret)
     ALLOCATE(TMIN3(IM,JM),RHMIN3(IM,JM),STAT=kret)
      IF ( trim(CORE) .EQ. 'GFS' .AND. FHR .GT. FHRHRLY .AND. LCYCON) THEN
        print *, 'For GFS output and fhr > 12, skip 3hr max/min temps'
        GO TO 555
      ELSEIF (trim(CORE) .EQ. 'GFS' .AND. FHR .GE. 15 .AND. .NOT.LCYCON) THEN
        print *, 'For off cycle GFS output and fhr >= 15, skip 3hr max/min temps'
        GO TO 555
      ELSEIF (LHR3 .AND. FHR .NE. 0) THEN
       print *, 'computing maxmin3 for fhr',FHR
!----------------Make into subroutine CalcMAX
!       calcmax(psfc,thold,dhold,downt,downdew,tmax,tmin,rhmax,rhmin)
       TMAX3=SPVAL;TMIN3=SPVAL
       RHMAX3=SPVAL;RHMIN3=SPVAL
       DO J=1,JM
       DO I=1,IM
        if(validpt(i,j)) then
          TMAX3(I,J)=-1*SPVAL
          RHMAX3(I,J)=-1*SPVAL
          IF (LHR12) THEN
           TMPT=THOLD(I,J,1)
           TMPD=DHOLD(I,J,1)
          ELSE
           TMPT=DOWNT(I,J)
           TMPD=DOWNDEW(I,J) 
          ENDIF

          THOLD(I,J,1)=DOWNT(I,J)
          DHOLD(I,J,1)=DOWNDEW(I,J)
          DO L=1,3
           IF(THOLD(I,J,L).GT.TMAX3(I,J).AND.THOLD(I,J,L).GT.1.0) &
             TMAX3(I,J)=THOLD(I,J,L)
           IF(THOLD(I,J,L).LT.TMIN3(I,J).AND.THOLD(I,J,L).GT.1.0) &
             TMIN3(I,J)=THOLD(I,J,L)
           QX=CalcQ(psfc(i,j),dhold(i,j,l))
           QSX=CalcQ(psfc(i,j),thold(i,j,l))
           RELH=100*QX/QSX
           IF(RELH.GT.RHMAX3(I,J)) RHMAX3(I,J)=RELH
           IF(RELH.LT.RHMIN3(I,J)) RHMIN3(I,J)=RELH
          ENDDO

! switch back the thold and dhold values since we need 
! the originals for the 12-hr values
          THOLD(I,J,1)=TMPT
          DHOLD(I,J,1)=TMPD
         endif
       ENDDO
       ENDDO

       CALL BOUND(RHMAX3,0.,100.)
       CALL BOUND(RHMIN3,0.,100.)
 
       where(tmin3.eq.0)tmin3=spval
      DEC=-2.0

       CALL FILL_FLD(GFLD8,NUMV,IM,JM,TMAX3)

       GFLD8%discipline=0
       GFLD8%ipdtnum=8     ! should be superfluous

       GFLD8%ipdtmpl(1)=0
!correct       GFLD8%ipdtmpl(2)=0
       GFLD8%ipdtmpl(2)=4
       GFLD8%ipdtmpl(9)=FHR3
!      GFLD8%ipdtmpl(10)=103
!      GFLD8%ipdtmpl(12)=2
       GFLD8%ipdtmpl(10)=1
       GFLD8%ipdtmpl(12)=0

       GFLD8%ipdtmpl(22)=1
!      GFLD8%ipdtmpl(24)=2
       GFLD8%ipdtmpl(24)=1
       GFLD8%ipdtmpl(27)=3

       CALL set_scale(gfld8, DEC)
       CALL PUTGB2(70,GFLD8,IRET) ! TMAX3

       CALL FILL_FLD(GFLD8,NUMV,IM,JM,TMIN3)

       GFLD8%discipline=0
       GFLD8%ipdtnum=8     ! should be superfluous

       GFLD8%ipdtmpl(1)=0
!correct       GFLD8%ipdtmpl(2)=0
       GFLD8%ipdtmpl(2)=5
       GFLD8%ipdtmpl(9)=FHR3
!      GFLD8%ipdtmpl(10)=103
!      GFLD8%ipdtmpl(12)=2
       GFLD8%ipdtmpl(10)=1
       GFLD8%ipdtmpl(12)=0

       GFLD8%ipdtmpl(22)=1
       GFLD8%ipdtmpl(24)=1
       GFLD8%ipdtmpl(27)=3

       CALL set_scale(gfld8, DEC)
       CALL PUTGB2(70,GFLD8,IRET) ! TMIN3

       CALL FILL_FLD(GFLD8,NUMV,IM,JM,RHMAX3)

       GFLD8%discipline=0
       GFLD8%ipdtnum=8     ! should be superfluous

       GFLD8%ipdtmpl(1)=1
       GFLD8%ipdtmpl(2)=27 ! maxrh
       GFLD8%ipdtmpl(9)=FHR3
!      GFLD8%ipdtmpl(10)=103
!      GFLD8%ipdtmpl(12)=2
       GFLD8%ipdtmpl(10)=1
       GFLD8%ipdtmpl(12)=0

       GFLD8%ipdtmpl(22)=1
!      GFLD8%ipdtmpl(24)=2 ! 2=max
       GFLD8%ipdtmpl(24)=1 ! 2=max
       GFLD8%ipdtmpl(27)=3

       CALL set_scale(gfld8, DEC)
       CALL PUTGB2(70,GFLD8,IRET) ! RHMAX3

       CALL FILL_FLD(GFLD8,NUMV,IM,JM,RHMIN3)

       GFLD8%discipline=0
       GFLD8%ipdtnum=8     ! should be superfluous

       GFLD8%ipdtmpl(1)=1
       GFLD8%ipdtmpl(2)=198 ! minrh
       GFLD8%ipdtmpl(9)=FHR3
!      GFLD8%ipdtmpl(10)=103
!      GFLD8%ipdtmpl(12)=2
       GFLD8%ipdtmpl(10)=1
       GFLD8%ipdtmpl(12)=0

       GFLD8%ipdtmpl(22)=1
!      GFLD8%ipdtmpl(24)=3 ! 3=min
       GFLD8%ipdtmpl(24)=1 ! 3=min
       GFLD8%ipdtmpl(27)=3

       CALL set_scale(gfld8, DEC)
       CALL PUTGB2(70,GFLD8,IRET) ! RHMIN3

      ENDIF  !LHR3
 555 CONTINUE

!  now compute the max and min values if end of 12-hr period
      ALLOCATE(TMAX12(IM,JM),RHMAX12(IM,JM),STAT=kret)
      ALLOCATE(TMIN12(IM,JM),RHMIN12(IM,JM),STAT=kret)
      IF (LHR12 .AND. FHR.NE.0) THEN 
        print *, '12-hr max min',FHR
!----------------Make into subroutine CalcMAX
!       calcmax(psfc,thold,dhold,downt,downdew,tmax,tmin,rhmax,rhmin)kj
        TMAX12=SPVAL;TMIN12=SPVAL 
        RHMAX12=SPVAL;RHMIN12=SPVAL
        THOLD(:,:,1)=DOWNT;DHOLD(:,:,1)=DOWNDEW
        DO J=1,JM
        DO I=1,IM
         if (validpt(i,j)) then
          TMAX12(I,J)=-SPVAL;RHMAX12(I,J)=-SPVAL 
          DO L=1,12
            IF(THOLD(I,J,L).GT.TMAX12(I,J).AND.THOLD(I,J,L).GT.1.0) &
              TMAX12(I,J)=THOLD(I,J,L)
            IF(THOLD(I,J,L).LT.TMIN12(I,J).AND.THOLD(I,J,L).GT.1.0) &
              TMIN12(I,J)=THOLD(I,J,L)
            QX=CalcQ(psfc(i,j),dhold(i,j,l))
            QSX=CalcQ(psfc(i,j),thold(i,j,l))
            RELH=100*QX/QSX
            IF(RELH.GT.RHMAX12(I,J)) RHMAX12(I,J)=RELH
            IF(RELH.LT.RHMIN12(I,J)) RHMIN12(I,J)=RELH
          ENDDO
         endif
        ENDDO
        ENDDO
        CALL BOUND(RHMAX12,0.,100.)
        CALL BOUND(RHMIN12,0.,100.)

         DEC=-2.0

       CALL FILL_FLD(GFLD8,NUMV,IM,JM,TMAX12)

       GFLD8%discipline=0
       GFLD8%ipdtnum=8     ! should be superfluous

       GFLD8%ipdtmpl(1)=0
!correct       GFLD8%ipdtmpl(2)=0
       GFLD8%ipdtmpl(2)=4
       GFLD8%ipdtmpl(9)=FHR12
!      GFLD8%ipdtmpl(10)=103
!      GFLD8%ipdtmpl(12)=2
       GFLD8%ipdtmpl(10)=1
       GFLD8%ipdtmpl(12)=0

       GFLD8%ipdtmpl(22)=1
!      GFLD8%ipdtmpl(24)=2
       GFLD8%ipdtmpl(24)=1
       GFLD8%ipdtmpl(27)=12

       CALL set_scale(gfld8, DEC)
       CALL PUTGB2(70,GFLD8,IRET) ! TMAX12

!       1-28-13 JTM : check for incorrect tmin even for validpt=true 
        where(tmin12.le.10)tmin12=spval

       CALL FILL_FLD(GFLD8,NUMV,IM,JM,TMIN12)

        write(0,*) 'minval(TMIN12): ', minval(TMIN12)
        write(0,*) 'maxval(TMIN12): ', maxval(TMIN12)

       GFLD8%discipline=0
       GFLD8%ipdtnum=8     ! should be superfluous

       GFLD8%ipdtmpl(1)=0
!tst       GFLD8%ipdtmpl(2)=0
       GFLD8%ipdtmpl(2)=5
       GFLD8%ipdtmpl(9)=FHR12
!      GFLD8%ipdtmpl(10)=103
!      GFLD8%ipdtmpl(12)=2
       GFLD8%ipdtmpl(10)=1
       GFLD8%ipdtmpl(12)=0

       GFLD8%ipdtmpl(22)=1
!      GFLD8%ipdtmpl(24)=3
       GFLD8%ipdtmpl(24)=1
       GFLD8%ipdtmpl(27)=12

       CALL set_scale(gfld8, DEC)
       CALL PUTGB2(70,GFLD8,IRET) ! TMIN12

         DEC=3.0

       CALL FILL_FLD(GFLD8,NUMV,IM,JM,RHMAX12)

       GFLD8%discipline=0
       GFLD8%ipdtnum=8     ! should be superfluous

       GFLD8%ipdtmpl(1)=1
       GFLD8%ipdtmpl(2)=27 ! rhmax
       GFLD8%ipdtmpl(9)=FHR12
!      GFLD8%ipdtmpl(10)=103
!      GFLD8%ipdtmpl(12)=2
       GFLD8%ipdtmpl(10)=1
       GFLD8%ipdtmpl(12)=0

       GFLD8%ipdtmpl(22)=1
!      GFLD8%ipdtmpl(24)=2 ! 2=max
       GFLD8%ipdtmpl(24)=1 ! 2=max
       GFLD8%ipdtmpl(27)=12

       CALL set_scale(gfld8, DEC)
       CALL PUTGB2(70,GFLD8,IRET) ! RHMAX12

       CALL FILL_FLD(GFLD8,NUMV,IM,JM,RHMIN12)

       GFLD8%discipline=0
       GFLD8%ipdtnum=8     ! should be superfluous

       GFLD8%ipdtmpl(1)=1
       GFLD8%ipdtmpl(2)=198 ! minrh
       GFLD8%ipdtmpl(9)=FHR12
!      GFLD8%ipdtmpl(10)=103
!      GFLD8%ipdtmpl(12)=2
       GFLD8%ipdtmpl(10)=1
       GFLD8%ipdtmpl(12)=0

       GFLD8%ipdtmpl(22)=1
!      GFLD8%ipdtmpl(24)=3 ! 3=min
       GFLD8%ipdtmpl(24)=1 ! 3=min
       GFLD8%ipdtmpl(27)=12

       CALL set_scale(gfld8, DEC)
       CALL PUTGB2(70,GFLD8,IRET) ! RHMIN12

      ENDIF !LHR12

        call baclose(70,iret)

      print *, 'completed main'
      STOP
      END PROGRAM smartinit

      SUBROUTINE SKYCVR(SKY,CFR,GDIN)
        use grddef
!----------------------------------------------------------------
!  Sky - Calculates cloud percentage in each layer based on
!        RH in that layer.  Then adds up the percentages in
!        the layers. Model clouds seem too 'binary', and so
!        they are not used.
!        We guess that it takes higher RH near the surface (say
!        97%) to get a cloud, but less RH up high (say only 90%
!        to get cirrus).  Transition width is wider up high, than
!        it is near the surface.
!        Also weight high clouds less in the coverage than
!        low clouds.
!        No downscaling is attempted since an observer can usually
!        see MANY gridpoints - and judges clouds based on all of
!        them - not just whether there is a cloud in the small
!        gridpoint directly overhead.  Thus, cloud fields are
!        rather smooth.
!----------------------------------------------------------------

!      remove surface level - so surface Fog does not count

!      get weight based on pressure - high levels get counted little
!      maxes out at 700mb, low levels count a little less

        REAL TSKY(100)
        TYPE (GINFO),INTENT(IN) :: GDIN
        REAL,    INTENT(IN)  :: CFR(:,:,:)
        REAL,    INTENT(INOUT) :: SKY(:,:)

!      When level 1 has 50% coverage, then 50% coverage
!      at level 2 covers 50% of the remaining clear sky,
!      (so now 75%) and 50% coverage at level 3 covers
!      50% of the remaining clear sky (now 87%), etc.
        
        SKY=0.
        IM=GDIN%imax;JM=GDIN%jmax;KMAX=GDIN%kmax

        DO J=1,JM
        DO I=1,IM
         TSKY=0.
         IL=1
!        Don't count 1st level fog
         L=2; M=L
         DO WHILE (IL .LE. 7) 
           IF(L.GE.KMAX .OR. M.GE.KMAX) GOTO 369   
           DO WHILE (L .LE. KMAX)
!              print *,'L=',L,CFR(I,J,L)

             IF (CFR(I,J,L).GT.0.) THEN
               TSKY(IL)=CFR(I,J,L)
               DO M=L,KMAX
!                 print *,'M ',M,CFR(I,J,M)
                 IF (CFR(I,J,M).EQ.0.) THEN 
                   TSKY(IL)=TSKY(IL)/100.
                   IL=IL+1 
                   L=M
                   EXIT    
                 ELSEIF (CFR(I,J,M).GT.TSKY(IL)) THEN
                   TSKY(IL)=CFR(I,J,M)
                 ENDIF 
               ENDDO
!              print *,I,J,'IL=',IL-1,' TSKY ',TSKY(IL-1)
             ENDIF

             L=L+1
           ENDDO 
         ENDDO 

369      SKY(I,J)=(1.-(1.-TSKY(1))*(1.-TSKY(2))*(1.-TSKY(3))*(1.-TSKY(4))*  &
                  (1.-TSKY(5))*(1.-TSKY(6))*(1.-TSKY(7)))*100.

        ENDDO
        ENDDO       

        RETURN
        END SUBROUTINE skycvr

        SUBROUTINE SNOWFALL(SN0,SNOWAMT,DOWNT,THOLD,GDIN,AVG,VALIDPT)
        use grddef
!===========================================================================
!  SnowAmt - simple snow ratio based on surface temperature - multiplied
!            times the model QPF amount
!      GSM - using snow liquid directly from the model instead but still
!            constructing a snow ratio;  using averaged sfc temp over period
!      SNOWAMT should use more temperature data,
!      but for now, it's the best guess at the avg temp over the 6 hour period
!---------------------------------------------------------------------------

        TYPE (GINFO), INTENT(IN) :: GDIN
        REAL,    INTENT(IN)    :: DOWNT(:,:), THOLD(:,:,:),SN0(:,:)
        REAL,    INTENT(INOUT) :: SNOWAMT(:,:)
        REAL,    ALLOCATABLE   :: TEMP1(:,:),TEMP2(:,:)
        LOGICAL, INTENT(IN)    :: VALIDPT(:,:)
        IM=GDIN%IMAX;JM=GDIN%JMAX

        ALLOCATE (TEMP1(IM,JM),TEMP2(IM,JM),STAT=kret)
        
         TEMP2=0.
         idiv=1
         IF (avg.gt.3.) idiv=2    !for 6 hr snow depths
        
         WHERE (validpt) 
         WHERE (SN0.GT.0.) 
           TEMP1=(DOWNT(:,:)+THOLD(:,:,2)+idiv*THOLD(:,:,3))/AVG  !TAVG
           WHERE (TEMP1.LT.264.)     ! using newwer nest codes
             TEMP2=20.
           ELSEWHERE
             TEMP2=(273.15-TEMP1)+8.   !SNOWR using newer nest codes 
           ENDWHERE
         END WHERE
        
         SNOWAMT=SN0*TEMP2*0.001            !Convert to m
         WHERE (SNOWAMT.LT.0) SNOWAMT=0.    ! Added for alaskanest for non-valid pt
         endwhere
          
         DEALLOCATE (TEMP1,TEMP2,STAT=kret)
 
        RETURN 
        END SUBROUTINE snowfall

        SUBROUTINE MKPOP(PBLMARK,RH,BLI,PCP01,PCP10,PXCP01,PXCP10,QPF,POP,GDIN,IAHR,VALIDPT)
        use grddef
!-------------------------------------------------------------------------
! PoP - based strongly on QPF (since when model has one inch of precip the
!   chance of getting 0.01 is pretty high).  However, there is a big
!   difference between a place that model has 0.00 precip and is very
!   close to precipitating - and those where model has 0.00 and is a
!   thousand miles from the nearest cloud.  Thus, uses the average
!   boundary layer RH to make an adjustment on the low end - adding
!   to PoP where RH is high.  Ignores surface RH to try to ignore fog
!  cases. Would also like to consider omega.

!   Uses hyperbolic tangent of QPF, so that it rises quickly as model
!   QPF increases - but tapers out to nearly 100% as QPF gets high.
!   Also uses hyperbolic tangent of QPF to reduce the impact of high RH
!   as QPF gets higher (since avg RH will always be high when QPF is high)

!   Adjustable parameters:
!     QPFMAX is QPF amount that would give 75% PoP if nothing else
!       considered at half this amount, PoP is 45%, at double this
!       amount PoP is 96%.  Default set at 0.40.
!     RHexcess is amount of average BL RH above which PoP is adjusted
!       upward. Default set to 60%
!     adjAmount is maximum amount of adjustment if BL RH is
!       totally saturated. Default set to 30%

! GSM   The above discussion is the original PoP methodology of the smarinit
!     code.   The problem is that in old days of the Eta with its dry bias,
!     when the model was able to generate very heavy amounts, it usually did
!     so with good reason.   In the current era of the high-res WRF, you can
!     now get very localized bullseyes of heavy QPF (often convective) that
!     don't verify.   This leads to localized bullseyes of PoP.   To generate
!     a more realistic field, I introduce PoP from the SREF and blend it with
!     the smartinit method to maintain some continuity with the NAM QPF.

!--------------------------------------------------------------------------

        TYPE (GINFO), INTENT(IN) :: GDIN
        REAL,    INTENT(IN)    :: PBLMARK(:,:), RH(:,:,:),BLI(:,:),QPF(:,:)
        REAL,    INTENT(INOUT) :: PCP01(:,:),PCP10(:,:),PXCP01(:,:),PXCP10(:,:)
        REAL,    INTENT(OUT)   :: POP(:,:)
        REAL,    ALLOCATABLE   :: TMPPCP(:,:)
        LOGICAL, INTENT(IN)    :: VALIDPT(:,:)
        INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200),ID(25)

      
        QPFMAX=0.40    ! QPF valuewhere raw PoP would be 75%
        RHexcess=60.0  ! RH above this can add to PoP and below will subtract
        adjAmount=30.0 ! amount of adjustment allowed

!--------------------------------------------------------------------------------
!  due to interpolation from coarse to fine grid, the 3-hr pop can end up
!  higher than the 12-hr pop at the same grid point.  Even it out if this occurs.
!-------------------------------------------------------------------------------- 
      IM=GDIN%imax;JM=GDIN%jmax;IFHR=GDIN%FHR
      IFHR6=IFHR-6

      print *,'Compute ',IAHR,' HR BUCKET    FHR=',IFHR 

      IF (IAHR.EQ.3 .AND. IFHR .GT. 11) THEN
        ALLOCATE(TMPPCP(IM,JM))
        WHERE (validpt .and. PCP01 .GT. PXCP01)
          TMPPCP=(PCP01+PXCP01)/2.
          PCP01  = TMPPCP
          PXCP01 = TMPPCP
        END WHERE

        WHERE (validpt .and. PCP10 .GT. PXCP10)
          TMPPCP=(PCP10+PXCP10)/2.      ! ERROR FOUND 09/26/13
          PCP10  = TMPPCP
          PXCP10 = TMPPCP
        END WHERE
        DEALLOCATE(TMPPCP)
      ENDIF

      DO I = 1, IM
      DO J = 1, JM
       if(validpt(i,j)) then
        LMBL=INT(PBLMARK(I,J))
        rhavg=100*SUM(rh(i,j,1:lmbl))/lmbl
        POPTMP=calcw(qpf(i,j),lmbl,rhavg,qpfmax,rhexcess,adjAmount)

!--------------------------------------------------------------------------------
! GSM  changed pop3 for stable/convective environments
!       based on the best LI.   any NAM amounts in non-convective
!       regimes are generally more believable so do not need to
!       "verify" chances with SREF probs of higher amounts;  will
!       use higher SREF thresholds for convective regimes.

!  GSM  For 6 and 12 hr POP, not using the same test on liftex index as in 3-hr POP
!     as an instantaneous value doesn't reflect a longer period
!     very well.  probably need to tie in convective precip at
!     some point.  very possible that POP6 or POP12 can end up less than
!     POP6 or POP3 since the computations are different - this isn't ideal,
!     but setting the POP12 or POPi6 to be no lower than the POP6,POP3 covers it
!--------------------------------------------------------------------------------
        BL=BLI(I,J) 

! GSM  modifed this to be more simple for Alaska.
!   for other regions, GSM recommends to weight the SREF prob of 0.10"
!    higher to balance the high PoP given by convective
!    bullseyes from the model.   But since AK is less prone
!    to model-generated convective bullseyes, and IC's are
!    so important here, opted to give more weight to the SREF
!    Using the Nested grid conus calculations  (jtm)

        if (gdin%region .eq. 'AK' .or. gdin%region .eq. 'AKRT' .or. gdin%region .eq. 'AK3' ) then
         IF (POPTMP .LT. 30.)  THEN
           POP(I,J)=(POPTMP+PCP01(I,J))/2.
         ELSE
           POP(I,J)=(POPTMP+2*PCP01(I,J)+PCP10(I,J))/4.
         ENDIF

        else 
        
         IF (IAHR.EQ.3) THEN
          IF (BL .GT. 0.) THEN
            POP(I,J)=(POPTMP+PCP01(I,J))/2.
          ELSE
            IF (POPTMP .GT. 70.) THEN
              POP(I,J)=(POPTMP+PCP01(I,J)+PCP10(I,J))/3.
            ELSE
              POP(I,J)=(2*POPTMP+2*PCP01(I,J)+PCP10(I,J))/5.
            ENDIF
          ENDIF
         ELSE 
          IF(POPTMP .GT. 70.) POP(I,J)=(2*POPTMP+2*PCP01(I,J)+PCP10(I,J))/5.
          IF(POPTMP .LT. 30.) POP(I,J)=(POPTMP+2*PCP01(I,J)+PCP10(I,J))/4.
          IF(POPTMP.GE.30 .AND. POPTMP.LE.70) POP(I,J)=AMAX1((POPTMP+PCP01(I,J))/2.,20.)
         ENDIF

         endif  !alaska domain check
        endif  !validpt check
       ENDDO
      ENDDO

      print *,'POP,QPF,PCP01,PCP10,BLI ', IAHR,POP(90,65), QPF(90,65),PCP01(90,65), &
               PCP10(90,65),BLI(90,65)
      RETURN 
      END SUBROUTINE mkpop

      SUBROUTINE GRIBLIMITED(SKY,IUNIT,GDIN,GFLD,IM,JM)
      use grddef
      use aset2d
      use asetdown
      USE GRIB_MOD
!---------------------------------------------------------
!  write limited data to grib file for hrs 1,2,4,5,7,8,10,11...fhrhrly
!  FOR Alaska,  this file serve as 1st guess for RTMA (akrtmages)
!---------------------------------------------------------
       INTEGER ID(25)
       LOGICAL RITEHD
       TYPE (GINFO) :: GDIN
       TYPE (GRIBFIELD):: GFLD, GFLD8
       REAL,    INTENT(INOUT)  :: SKY(:,:)

    INCLUDE 'DEFGRIBINT.INC'   ! interface statements for gribit subroutines

       print *,'OUTPUT LIMITED GRIB FILE at FHR ',GDIN%FHR,' for REGION ',GDIN%REGION
       RITEHD = .TRUE.

       NUMV=IM*JM
       DEC=-2.0
        CALL FILL_FLD(GFLD,NUMV,IM,JM,DOWNT)

       GFLD%ipdtnum=0
       GFLD%ipdtmpl(1)=0
       GFLD%ipdtmpl(2)=0
       GFLD%ipdtmpl(9)=gdin%FHR
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0

       GFLD%idrtnum=40 ! 40 = JPEG
       GFLD%idrtmpl(5)=0
       GFLD%idrtmpl(6)=0
       GFLD%idrtmpl(7)=-1

       gfld%idrtmpl(1)=0


       CALL set_scale(gfld, DEC)
        write(0,*) 'back from set_scale'
       CALL PUTGB2(70,GFLD,IRET) ! DOWNT


! ----------------------------------------

       DEC=-2.0

       CALL FILL_FLD(GFLD,NUMV,IM,JM,DOWNDEW)

       GFLD%ipdtmpl(1)=0
       GFLD%ipdtmpl(2)=6
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0
       gfld%idrtmpl(2)=DEC

       CALL set_scale(gfld, DEC)
       CALL PUTGB2(70,GFLD,IRET) ! DOWNDEW


       DEC=6.0
       CALL FILL_FLD(GFLD,NUMV,IM,JM,DOWNQ)

       GFLD%ipdtmpl(1)=1
       GFLD%ipdtmpl(2)=0
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0

        write(0,*) 'scale and write DOWNQ'
       CALL set_scale(gfld, DEC)
       CALL PUTGB2(70,GFLD,IRET)  ! DOWNQ

! ----------------------------------------

       DEC=-2.0

       CALL FILL_FLD(GFLD,NUMV,IM,JM,DOWNU)

       GFLD%ipdtmpl(1)=2
       GFLD%ipdtmpl(2)=2
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0

       CALL set_scale(gfld, DEC)
       CALL PUTGB2(70,GFLD,IRET) ! DOWNU

       DEC=-2.0

       print *, 'DOWNU',minval(downu),maxval(downu)
       print *, 'DOWNV',minval(downv),maxval(downv)

       CALL FILL_FLD(GFLD,NUMV,IM,JM,DOWNV)

       GFLD%ipdtmpl(1)=2
       GFLD%ipdtmpl(2)=3
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0
        write(0,*) 'gfld%idrtmpl: ', gfld%idrtmpl

       CALL set_scale(gfld, DEC)
       CALL PUTGB2(70,GFLD,IRET) ! DOWNV

       DEC=3.0

       CALL FILL_FLD(GFLD,NUMV,IM,JM,PSFC)

       GFLD%ipdtmpl(1)=3
       GFLD%ipdtmpl(2)=0
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0

       CALL set_scale(gfld, DEC)
       CALL PUTGB2(70,GFLD,IRET) ! PSFC

         DEC=-2.0

       CALL FILL_FLD(GFLD,NUMV,IM,JM,TOPO)

       GFLD%ipdtmpl(1)=3
       GFLD%ipdtmpl(2)=5
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0

       CALL set_scale(gfld, DEC)
       CALL PUTGB2(70,GFLD,IRET)  ! TOPO

         DEC=1.0

       CALL FILL_FLD(GFLD,NUMV,IM,JM,VEG_NDFD)

       GFLD%discipline=2

       GFLD%ipdtmpl(1)=0
       GFLD%ipdtmpl(2)=0 ! was 198
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0

        write(0,*) 'min,max(VEG_NDFD): ', minval(VEG_NDFD), &
                                          maxval(VEG_NDFD)

       CALL set_scale(gfld, DEC)
       CALL PUTGB2(70,GFLD,IRET) ! VEG_NDFD

! 03-19-13 : Add Gust and visibility to limited files for RTMA
! 06-03-15 : Add Cloud Ceiling to limited files for RTMA
! 08-12-15 : Add Sky Cover to limited files for RTMA
! 11-07-15 : Add SLP and SST to limited files for RTMA
      IF (trim(GDIN%CORE) .NE. 'GFS') THEN

       DEC=3.0

       CALL FILL_FLD(GFLD,NUMV,IM,JM,WGUST)

       GFLD%discipline=0

       GFLD%ipdtmpl(1)=2
       GFLD%ipdtmpl(2)=22
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0

       CALL set_scale(gfld, DEC)
       CALL PUTGB2(70,GFLD,IRET) ! WGUST

! VISIBILITY

      DEC=2.7

       CALL FILL_FLD(GFLD,NUMV,IM,JM,VIS)

       GFLD%discipline=0
       GFLD%ipdtnum=0
       GFLD%ipdtmpl(1)=19
       GFLD%ipdtmpl(2)=0
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0

       CALL set_scale(gfld, DEC)
       CALL PUTGB2(70,GFLD,IRET)  ! VIS
        write(0,*) 'IRET for VIS: ', IRET

! SKY COVER
        print*, 'Output Sky Cover', GDIN%FHR
                DEC=3.0

        CALL FILL_FLD(GFLD,NUMV,IM,JM,SKY)

       GFLD%discipline=0
       GFLD%ipdtnum=0
       GFLD%ipdtmpl(1)=6
       GFLD%ipdtmpl(2)=1
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0

        CALL set_scale(gfld, DEC)
        CALL PUTGB2(70,GFLD,IRET) ! SKY

        print*,'maxval(SKY),minval(SKY): ', maxval(SKY),minval(SKY)

! CLOUD CEILING HEIGHT
      IF (trim(GDIN%CORE) .NE. 'dgx') THEN
      print*, 'Output Cloud Ceiling Height', GDIN%FHR

      DEC=-5.0
       CALL FILL_FLD(GFLD,NUMV,IM,JM,CEIL)

       GFLD%discipline=0
       GFLD%ipdtnum=0
       GFLD%ipdtmpl(1)=3
       GFLD%ipdtmpl(2)=5
       GFLD%ipdtmpl(10)=215
       GFLD%ipdtmpl(12)=0

       CALL set_scale(gfld, DEC)
       CALL PUTGB2(70,GFLD,IRET)  ! VIS
        write(0,*) 'IRET for CEIL: ', IRET
        write(0,*) 'maxval(CEIL),minval(CEIL): ', maxval(CEIL),minval(CEIL)

! SLP
      print*, 'Output SLP', GDIN%FHR
! MSLET (Mesinger/Membrane)
      DEC=-0.1

       CALL FILL_FLD(GFLD,NUMV,IM,JM,SLP)

       GFLD%discipline=0
       GFLD%ipdtnum=0
       GFLD%ipdtmpl(1)=3
       GFLD%ipdtmpl(2)=192
       GFLD%ipdtmpl(10)=101
       GFLD%ipdtmpl(12)=0

       CALL set_scale(gfld, DEC)
       CALL PUTGB2(70,GFLD,IRET)  ! SLP

      print*,'maxval(SLP),minval(SLP): ', maxval(SLP),minval(SLP)

! SST - this is really Skin T/SST, but we are writing it out as 2-m Temperature,
! since Skin T/SST is already being used (per Geoff DiMego).
! Disable for this implementation - revisit for next implementation
      print*, 'Output SST', GDIN%FHR
      DEC=-2.0

       CALL FILL_FLD(GFLD,NUMV,IM,JM,SST)

       GFLD%discipline=0
       GFLD%ipdtnum=0
       GFLD%ipdtmpl(1)=0
       GFLD%ipdtmpl(2)=0
       GFLD%ipdtmpl(10)=103
       GFLD%ipdtmpl(12)=2

       CALL set_scale(gfld, DEC)
       CALL PUTGB2(70,GFLD,IRET)  ! SST
        write(0,*) 'IRET for SST: ', IRET
        write(0,*) 'maxval(SST),minval(SST): ', maxval(SST),minval(SST)

      endif ! dgx
      ENDIF

       return
       END SUBROUTINE griblimited
   
   REAL FUNCTION CalcQ(ptmp,ttmp)
   use constants
      REAL, INTENT(IN) :: ptmp,ttmp
      CalcQ=pq0/ptmp*EXP(A2*(ttmp-A3)/(ttmp-A4)) 
   END FUNCTION calcq      

   REAL FUNCTION calcw(qpftmp,lmbl,rhavg,qpfmax,RHexcess,adjAmount)
!----------------------------------------------------------------------
!      -Compute Cloud Water
!       Adjustable parameters:
!         QPFMAX should be higher than PoP topQPF
!          Default set at 0.60.
!         RHexcess should be higher than PoP RHexcess
!          Default set to 80%
!         adjAmount should be smaller than PoP adjAmount
!          Default set to 10%
!----------------------------------------------------------------------
      REAL, INTENT(IN) :: qpftmp,rhavg,qpfmax,RHexcess,adjAmount
      INTEGER, INTENT(IN) :: lmbl

      factor=tanh(QPFtmp*(1.0/QPFMAX))
      factor2=tanh(QPFtmp*(2.0/QPFMAX))
      rhmx=100-RHexcess
      DPOP=rhavg-RHexcess
      IF(DPOP.LT.0.) DPOP=0.
      dpop=(dpop/rhmx)*(1.0-factor2)*adjAmount
      calCW=(factor*100.0)+dpop
   END FUNCTION calcw      

     SUBROUTINE HINDEX (IM,JM,HAINES,HLVL,VALIDPT)
!=======================================================================
!  Calculate Haines Index
!  type is "LOW", "MEDIUM", "HIGH"
!  NOTE, the default haines index calcaulation is defined by:
!  python self.whichHainesIndex, which can be set to "LOW", "MEDIUM", "HIGH".
!
!  From Haines,D.A. (1988, Nat.Wea.Dig,V.13,#2, pp.23-27) 
!  where amount of atmospheric stability and dryness are 
!  accounted for to estimate fire potential growth.
!
!  INPUT : T in K, RH in percent (0-100), Topo (m)
!
!  11-05-2013 J.T. McQueen
!  11-15-2013 Using standard elevations for P950, P850
!  02-03-2015 corrected dew point depression calculation 
!             and conversion of RHMOIS to 0-1 for TD calculation
!=======================================================================
      use aset2d
      use asetdown
      LOGICAL, INTENT(IN)   :: VALIDPT(:,:)
      INTEGER, INTENT(INOUT)    :: HAINES(:,:),HLVL(:,:)
      REAL slopet,slopem,hat,tmois,rhmois,term,dpmois,tddiff,intt,intm
      REAL hainesm,hainest
 
      print *, 'Computing HAINES INDEX', IM,JM
      
      DO J=1,JM
      DO I=1,IM
       if (validpt(i,j)) then
!jtm       IF(DOWNP(I,J).GT.95000.) THEN
       IF(TOPO(I,J).LT.540.) THEN  
        HAT=T950(I,J)-T850(I,J)
        TMOIS=T850(I,J)-273.15
        RHMOIS=RH850(I,J)
        ST1=8
        ST2=3
        MT1=10
        MT2=5
        HLVL(I,J)=1
!jtm       ELSE IF(DOWNP(I,J).GT.85000.) THEN
        ELSE IF(TOPO(I,J).LT. 1456.) THEN
        HAT=T850(I,J)-T700(I,J)
        TMOIS=T850(I,J)-273.15
        RHMOIS=RH850(I,J)
        ST1=11
        ST2=5
        MT1=13
        MT2=5
        HLVL(I,J)=2
       ELSE
        HAT=T700(I,J)-T500(I,J)
        TMOIS=T700(I,J)-273.15
        RHMOIS=RH700(I,J)
        ST1=22
        ST2=17
        MT1=21
        MT2=14
        HLVL(I,J)=3
       ENDIF

!      Compute Dew point depression
       RHMOIS=RHMOIS/100.
       TERM=log10(RHMOIS) / 7.5 + (TMOIS / (TMOIS + 237.3))
       DPMOIS=(TERM * 237.3) / (1.0 - TERM)
       TDDIFF=TMOIS-DPMOIS 

!      Compute contribution from thermal stability
       SLOPET=1./real(ST1-ST2)
!jtm       INTT=1.5-(ST2-0.5)*SLOPET  ! intercept for temperature Lapse rate
       INTT=1.5-(ST2+0.5)*SLOPET  ! intercept for temperature Lapse rate
       HAINEST=(SLOPET*HAT)+INTT
       HAINEST=amin1(HAINEST,3.0)
       HAINEST=amax1(HAINEST,1.0)

!      Compute contribution from atmospheric dryness
       SLOPEM=1./real(MT1-MT2)
!jtm       INTM=1.5-(MT2-0.5)*SLOPEM
       INTM=1.5-(MT2+0.5)*SLOPEM
!jtm    HAINESM=(SLOPEM*DPMOIS)+INTM
       HAINESM=(SLOPEM*TDDIFF)+INTM
       HAINESM=amin1(HAINESM,3.0)
       HAINESM=amax1(HAINESM,1.0)
       HAINEST=NINT(HAINEST)
       HAINESM=NINT(HAINESM)

!      Compute total Haines Index from stability and dryness
       HAINES(I,J)=INT(HAINEST)+INT(HAINESM)
       if (VEG_NDFD(I,J).LE.0..OR.VEG_NDFD(I,J).EQ.16) then 
          HAINES(I,J)=0
          HLVL(I,J)=0
       endif

!       if (i.gt.125 .and. i.lt.150) then
!       if (j.eq.200) then
!         print *,'HAINES ',i,j,HAINES(I,J),HLVL(i,j),RHMOIS
!         print *,'HIT',HAINEST,TMOIS,HAT
!         print *,'HIM',HAINESM,DPMOIS,TDDIFF
!       endif
!       endif

      endif
   
      ENDDO
      ENDDO
      RETURN
      END     
! -------------------------
        SUBROUTINE FILL_FLD(GFLD,NUMV,IM,JM,ARRAY2D)
        USE GRIB_MOD
        USE pdstemplates
        TYPE (GRIBFIELD)  :: GFLD
        INTEGER :: NUMV, IM, JM, KK
        REAL :: ARRAY2D(IM,JM)

        DO KK = 1, NUMV
          IF(MOD(KK,IM).EQ.0) THEN
            M=IM
            N=INT(KK/IM)
          ELSE
            M=MOD(KK,IM)
            N=INT(KK/IM) + 1
          ENDIF
          GFLD%FLD(KK)=ARRAY2D(M,N)
!        if (mod(KK,25000) .eq. 0) then
!        write(0,*) 'M,N, ARRAY2D from gfld: ', M,N, GFLD%FLD(KK)
!        endif
        ENDDO
        END SUBROUTINE FILL_FLD

! -------------------------
        SUBROUTINE SET_SCALE(GFLD,DEC)
        USE GRIB_MOD
        USE pdstemplates
        TYPE (GRIBFIELD)  :: GFLD
        LOGICAL*1, allocatable:: locbmap(:)
        real :: DEC



        allocate(locbmap(size(GFLD%fld)))

        if (GFLD%ibmap .eq. 0 .or. GFLD%ibmap .eq. 254) then
        locbmap=GFLD%bmap
        write(0,*) 'used GFLD bmap'
        else
        write(0,*) 'hardwire locbmap to true'
        locbmap=.true.
        endif

! INPUT
!   ibm: integer, bitmap flag (grib2 table 6.0)
!   scl: real, significant digits,OR binary precision if < 0
!   len: integer, field and bitmap length
!   bmap: logical(len), bitmap (.true.: keep, bitmap (.true.: keep, .false.
!   skip)
!   fld: real(len), datafield
! OUTPUT
!   ibs: integer, binary scale factor
!   ids: integer, decimal scale factor
!   nbits: integer, number of bits to pack



        call g2getbits(GFLD%ibmap,DEC,size(GFLD%fld),locbmap,GFLD%fld, &
                      GFLD%idrtmpl(1),GFLD%idrtmpl(2),GFLD%idrtmpl(3),GFLD%idrtmpl(4))

        write(0,*) 'gfld%idrtmpl(2:3) defined, inumbits: ', gfld%idrtmpl(2:4)

        END SUBROUTINE SET_SCALE

! --------------------------------

       subroutine g2getbits(ibm,scl,len,bmap,g,gmin,ibs,ids,nbits)
!$$$
!   This subroutine is changed from w3 lib getbit to compute the total number of
!   bits,
!   The argument list is modified to have ibm,scl,len,bmap,g,ibs,ids,nbits
!
!  Progrma log:
!    Jun Wang  Apr, 2010
!
! INPUT
!   ibm: integer, bitmap flag (grib2 table 6.0)
!   scl: real, significant digits,OR binary precision if < 0
!   len: integer, field and bitmap length
!   bmap: logical(len), bitmap (.true.: keep, bitmap (.true.: keep, .false.
!   skip)
!   fld: real(len), datafield
! OUTPUT
!   ibs: integer, binary scale factor
!   ids: integer, decimal scale factor
!   nbits: integer, number of bits to pack
!
      IMPLICIT NONE
!
      INTEGER,INTENT(IN)   :: IBM,LEN
      LOGICAL*1,INTENT(IN) :: BMAP(LEN)
      REAL,INTENT(IN)      :: scl,G(LEN)
      INTEGER,INTENT(OUT)  :: IBS,IDS,NBITS
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      INTEGER,PARAMETER    :: MXBIT=16
!
!  NATURAL LOGARITHM OF 2 AND 0.5 PLUS NOMINAL SAFE EPSILON
      real,PARAMETER :: ALOG2=0.69314718056,HPEPS=0.500001
!
!local vars
      INTEGER :: I,I1,icnt,ipo,le,irange
      REAL    :: GROUND,GMIN,GMAX,s,rmin,rmax,range,rr,rng2,po,rln2
!
      DATA       rln2/0.69314718/


! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  ROUND FIELD AND DETERMINE EXTREMES WHERE BITMAP IS ON
      IF(IBM == 255) THEN
        GMAX = G(1)
        GMIN = G(1)
        DO I=2,LEN
          GMAX = MAX(GMAX,G(I))
          GMIN = MIN(GMIN,G(I))
        ENDDO
      ELSE
        do i1=1,len
          if (bmap(i1)) exit
        enddo
!       I1 = 1
!       DO WHILE(I1 <= LEN .AND. .not. BMAP(I1))
!         I1=I1+1
!       ENDDO
        IF(I1 <= LEN) THEN
          GMAX = G(I1)
          GMIN = G(I1)
          DO I=I1+1,LEN
            IF(BMAP(I)) THEN
              GMAX = MAX(GMAX,G(I))
              GMIN = MIN(GMIN,G(I))
            ENDIF
          ENDDO
        ELSE
          GMAX = 0.
          GMIN = 0.
        ENDIF
      ENDIF
      write(0,*)' GMIN=',GMIN,' GMAX=',GMAX
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  COMPUTE NUMBER OF BITS
      icnt = 0
      ibs = 0
      ids = 0
      range = GMAX - GMIN
!      IF ( range .le. 0.00 ) THEN
      IF ( range .le. 1.e-30 ) THEN
        nbits = 8
        return
      END IF
!*
      IF ( scl .eq. 0.0 ) THEN
          nbits = 8
          RETURN
      ELSE IF ( scl  >  0.0 ) THEN
          ipo = INT (ALOG10 ( range ))
!jw: if range is smaller than computer precision, set nbits=8
          if(ipo<0.and.ipo+scl<-20) then
            print *,'for small range,ipo=',ipo,'ipo+scl=',ipo+scl,'scl=',scl
            nbits=8
            return
          endif

          IF ( range .lt. 1.00 ) ipo = ipo - 1
          po = float(ipo) - scl + 1.
          ids = - INT ( po )
          rr = range * 10. ** ( -po )
          nbits = INT ( ALOG ( rr ) / rln2 ) + 1
      ELSE
          ibs = -NINT ( -scl )
          rng2 = range * 2. ** (-ibs)
          nbits = INT ( ALOG ( rng2 ) / rln2 ) + 1
      END IF
!     write(0,*)'in g2getnits,ibs=',ibs,'ids=',ids,'nbits=',nbits,'range=',range
!*
      IF(nbits <= 0) THEN
        nbits = 0
        IF(ABS(GMIN) >= 1.) THEN
          ids = -int(alog10(abs(gmin)))
        ELSE IF (ABS(GMIN) < 1.0.AND.ABS(GMIN) > 0.0) THEN
          ids = -int(alog10(abs(gmin)))+1
        ELSE
          ids = 0
        ENDIF
      ENDIF
      nbits = min(nbits,MXBIT)
!     write(0,*)'in g2getnits ibs=',ibs,'ids=',ids,'nbits=',nbits
!
      IF ( scl > 0.0 ) THEN
        s=10.0 ** ids
        IF(IBM == 255) THEN
          GROUND = G(1)*s
          GMAX   = GROUND
          GMIN   = GROUND
          DO I=2,LEN
            GMAX = MAX(GMAX,G(I)*s)
            GMIN = MIN(GMIN,G(I)*s)
          ENDDO
        ELSE
          do i1=1,len
            if (bmap(i1)) exit
          enddo
 !        I1=1
 !        DO WHILE(I1.LE.LEN.AND..not.BMAP(I1))
 !          I1=I1+1
 !        ENDDO
          IF(I1 <= LEN) THEN
            GROUND = G(I1)*s
            GMAX   = GROUND
            GMIN   = GROUND
            DO I=I1+1,LEN
              IF(BMAP(I)) THEN
                GMAX = MAX(GMAX,G(I)*S)
                GMIN = MIN(GMIN,G(I)*S)
              ENDIF
            ENDDO
          ELSE
            GMAX = 0.
            GMIN = 0.
          ENDIF
        ENDIF

        range = GMAX-GMIN
        if(GMAX == GMIN) then
          ibs = 0
        else
          ibs = nint(alog(range/(2.**NBITS-0.5))/ALOG2+HPEPS)
        endif
!
      endif
        write(0,*) 'leave g2getbits with GMIN: ', GMIN
!        GFLD%idrtmpl(1)=GMIN
!     write(0,*)'in g2getnits,2ibs=',ibs,'ids=',ids,'nbits=',nbits,'range=',&
!                range, 'scl=',scl,'data=',maxval(g),minval(g)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END subroutine g2getbits
