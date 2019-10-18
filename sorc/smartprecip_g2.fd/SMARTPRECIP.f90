       PROGRAM SMARTPRECP

        USE GRIB_MOD

!                .      .    .                                       .
! SUBPROGRAM:    SMARTPECIP
!   PRGMMR: MANIKIN        ORG: W/NP22     DATE:  07-03-07

! ABSTRACT: PRODUCES 3,6 or 12-HOUR TOTAL AND CONVECTIVE PRECIPITATION BUCKETS
!              AS WELL AS SNOWFALL ON THE ETA NATIVE GRID FOR SMARTINIT 

! PROGRAM HISTORY LOG:
!   07-03-07  GEOFF MANIKIN 
!   10-25-12  JEFF MCQUEEN
!   05-16-15  ANNETTE GIBBS
!   06-28-15  Annette Gibbs - changes made to latest smartinit version
! REMARKS:
!   10-25-12 JTM UNIFIED make and add precip for different accum hours
!                addprecip6, addprecip12 and makeprecip all combined in
!                smartprecip
!                To call, must set all 4 fhrs
!                for 3 or 6 hour buckets, set fhr3,fh4 to -99
!                For 12 hour buckets: 
!                    smartprecip  fhr fhr-3 fhr-6 fhr-9 
!  02-20-14 JTM Added DGEX option addsub to compute 6 hr precip
!               from 3 dgex files
!               fhr1 (fhr) 3 hr precip
!               fhr2 (fh3 old) 6 hr precip
!               fhr3 (fh6 old) 3 hr precip
!                 then mk6p=fhr+(fhr3-fhr6) for DGEX
!               Added DGEX 12 hr precip option
!               fhr1=fhr6 6hr precip
!               fhr2=fhr  6hr precip
!                 12hrp=fhr6 + fhr

! ATTRIBUTES:
!   LANGUAGE: FORTRAN-90
!   MACHINE:  WCOSS     
!======================================================================
      INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)
      INTEGER SHR1,FHR1, FHR2, FHR3, FHR4
      CHARACTER*80 FNAME
      LOGICAL*1 LSUB, MK3P,MK6P,MK12P,LADDSUB,LRD3,LRD4

!C grib2
      INTEGER :: LUGB,LUGI,J,JDISC,JPDTN,JGDTN
      integer,dimension(200) :: jids,jpdt,jgdt
!     INTEGER,DIMENSION(:) :: JIDS(200),JPDT(200),JGDT(200)
!     INTEGER,DIMENSION(:) :: PDS_SNOW_HOLD(200),PDS_RAIN_HOLD(200)
!     INTEGER,DIMENSION(:) :: PDS_SNOW_HOLD_EARLY(200), &
!                             PDS_RAIN_HOLD_EARLY(200)
      integer, allocatable :: pds_rain_hold_early(:), pds_rain_hold(:)
      integer, allocatable :: drt_rain_hold_early(:), drt_rain_hold(:)
      integer, allocatable :: gds_rain_hold_early(:), gds_rain_hold(:)
      integer, allocatable :: pds_snow_hold_early(:), pds_snow_hold(:)
      integer, allocatable :: drt_snow_hold_early(:), drt_snow_hold(:)
      integer, allocatable :: gds_snow_hold_early(:), gds_snow_hold(:)

      LOGICAL :: UNPACK
      INTEGER :: K,IRET
      TYPE(GRIBFIELD) :: GFLD
!C grib2

      REAL,     ALLOCATABLE :: GRID(:)
      REAL,     ALLOCATABLE :: APCP1(:),APCP2(:),APCP3(:),APCP4(:)
      REAL,     ALLOCATABLE :: CAPCP1(:),CAPCP2(:),CAPCP3(:),CAPCP4(:)
      REAL,     ALLOCATABLE :: SNOW1(:),SNOW2(:),SNOW3(:),SNOW4(:)
      REAL,     ALLOCATABLE :: APCPOUT(:),CAPCPOUT(:),SNOWOUT(:)
      LOGICAL,  ALLOCATABLE :: MASK(:)

! Added for getbit
      real,     allocatable :: grnd(:)
      real :: gmin, gmax
! Needed for the Dell
!     integer,dimension(:) :: gds_hold(21)
!--------------------------------------------------------------------------

      FNAME='fort.  '
!====================================================================
!     FHR3 = -99 signals a 6 hour summation requested
!     FHR4 GT 00 signals a 12 hour summation requested
!     FHR3 GT 0 but FHR4 = -99 signals a 6 hour summation For DGEX (fhr1+(fhr2-fh3))
!     FHR1 GT FHR2 signals do a 3 hour subtraction of files
!     FHR2 GT FHR1 signals do an addition (eg: DGEX, fhr1+fhr2=6h pr)
!====================================================================
      READ (5,*) FHR1, FHR2,FHR3,FHR4
      print *,' SMARTPRECIP ', FHR1,FHR2,FHR3,FHR4

      MK3P=.FALSE.; MK6P=.FALSE.; MK12P=.FALSE.
      LRD3=.FALSE.;LRD4=.FALSE.
      IGDNUM=0
!==>  Make 3 hour buckets by subtracting the 1st file from the 2nd
      LSUB=.FALSE.

        writE(0,*) 'enter FHR1, FHR2, FHR3, FHR4: ', &
                          FHR1, FHR2, FHR3, FHR4

      LADDSUB=.FALSE.
      IF (FHR1.GT.FHR2) THEN
        write(0,*) 'subtracting'
        MK3P=.TRUE.
        SHR1=FHR2
        FHR2=FHR1   ! T
        FHR1=SHR1   ! T-3
        IF (FHR3.GT.0) SHR1=FHR3  ! DGEX, T-6
        LSUB=.TRUE.
        write(0,*) 'reset so SHR1, FHR1, FHR2 are: ', SHR1, FHR1, FHR2

!==>    Set SHR1 to read NAM Accumulated snowfall bucket : 3,6,9 or 12 hr snow
!         =initial hour for snow bucket (T-3, -6, -9, -12
        if (FHR3 .lt. 0) then    ! For NON-DGEX grids
!         NAM has 12 hr snow buckets at 00 and 12 UTC Valid times
          IF (MOD(FHR2,12).EQ. 0.) THEN   
            SHR1=FHR2-12
        write(0,*) 'SHR1 defined(a): ', SHR1
          ELSE
            SHR1=FHR2-MOD(FHR2,12) 
        write(0,*) 'SHR1 defined(b): ', SHR1
          ENDIF
        else
          SHR1=FHR2-6
        endif
        print *, 'SUB: Create 3 hr precip from two files'
        print *, ' FHR1=', FHR1,' FHR2=',FHR2,' Snow SHR1=',SHR1

      ELSE

!==>    ADD or ADDSUB: Make 6 hr precip 
        LSUB=.FALSE.
        if (FHR4.LT.0) then 
          MK6P=.TRUE.
          if (FHR3 .LT. 0) then   ! NAM grid
            SHR1=FHR1-3  ! T-6
            print *,'ADD: Create 6 hr precip from two files: '
            print *,'FHR1=',FHR1,' FHR2=',FHR2,'Snow SHR1=',SHR1
          else
            SHR1=FHR1-3  ! T-3
            LADDSUB=.TRUE.  ! T-6   DGEX grid, 3rd file 
            LRD3=.TRUE.
            print *,'ADDSUB: Create 6 hr precip from 3 files:'
            print *, 'FHR3:',FHR3,'+ (FHR2:',FHR2,' - FHR1:',FHR1,')'
          endif

        else 

!==>      ADD: make 12 hr precip 
          MK12P=.TRUE.
          if ( FHR3 .GT. FHR2 ) Then
            SHR1=FHR1-3  ! T-12
            print *, 'ADD: Create 12 hr precip from four 3-hr buckets'
            print *, 'FHR1=',FHR1,' FHR2=',FHR2,' FHR3=',FHR3,' FHR4=',FHR4
            print *, 'Snow SHR1=',SHR1
            LRD3=.TRUE.;LRD4=.TRUE.
          else
            SHR1=FHR1-6  ! T-12, Add 2 6 hr precip buckets
            print *, 'ADD: Create 12 hr precip from two 6-hr buckets'  
            print *, 'FHR1=',FHR1,' FHR2=',FHR2
            print *, 'Snow SHR1=',SHR1
            LRD3=.FALSE.;LRD4=.FALSE.
          endif
        endif
      ENDIF

      LUGB=13;LUGI=14; LUGB2=15;LUGI2=16
      LUGB3=17;LUGI3=18;LUGB4=19;LUGI4=20
      LUGB5=50; LUGB6=51; LUGB7=52
      ISTAT = 0

! -== GET SURFACE FIELDS ==-


!        allocate(gfld%fld(1200*1200))
!       allocate(gfld%idsect(200))
!       allocate(gfld%igdtmpl(200))
!       allocate(gfld%ipdtmpl(200))
!       allocate(gfld%idrtmpl(200))
!        allocate(gfld%bmap(1200*1200))
      allocate(gfld%ipdtmpl(29))
      allocate(gfld%igdtmpl(21))
      allocate (pds_rain_hold(29),pds_rain_hold_early(29))
      allocate (drt_rain_hold(5),drt_rain_hold_early(5))
      allocate (gds_rain_hold(21),gds_rain_hold_early(21))
      allocate (pds_snow_hold(29),pds_snow_hold_early(29))
      allocate (drt_snow_hold(5),drt_snow_hold_early(5))
      allocate (gds_snow_hold(21),gds_snow_hold_early(21))

        JIDS=-9999
        JPDTN=-1
        JPDT=-9999
        JGDTN=-1
        JGDT=-9999
        UNPACK=.false.

        WRITE(FNAME(6:7),FMT='(I2)')LUGB
        CALL BAOPENR(LUGB,FNAME,IRETGB)

        WRITE(FNAME(6:7),FMT='(I2)')LUGB2
        CALL BAOPENR(LUGB2,FNAME,IRETGB)

        write(0,*) 'trim(fname): ', trim(fname)

        write(0,*) 'IRETGB on BAOPEN: ', IRETGB

        call getgb2(LUGB2,LUGI2,0,0,JIDS,JPDTN,JPDT,JGDTN,JGDT, &
                    UNPACK,K,GFLD,IRET)

        write(0,*) 'IRET from init getgb2 call: ', IRET

        NUMVAL=gfld%ngrdpts

        write(0,*) 'NUMVAL ', NUMVAL

        if (IRET .ne. 0) STOP

        UNPACK=.true.

      ALLOCATE (MASK(NUMVAL),GRID(NUMVAL),STAT=kret)
      ALLOCATE (APCP1(NUMVAL),CAPCP1(NUMVAL),SNOW1(NUMVAL),STAT=kret)
      IF(kret.ne.0)THEN
       WRITE(*,*)'ERROR allocation source location: ',numval
       STOP
      END IF

!   PRECIP

        write(0,*) 'have NUMVAL : ', NUMVAL

        write(0,*) 'allocate again?'
        allocate(gfld%fld(NUMVAL))
        allocate(gfld%bmap(NUMVAL))

        JIDS=-9999
        JPDTN=8
        JPDT=-9999
        JPDT(2)=8
        JGDTN=-1
        JGDT=-9999
        UNPACK=.true.

      print *;print *,'FHR1= ',FHR1, ' READ 1st PRECIP FILE ', LUGB,LUGI

        call getgb2(LUGB,LUGI,0,0,JIDS,JPDTN,JPDT,JGDTN,JGDT, &
                    UNPACK,K,GFLD,IRET_EARLY)

        write(0,*) 'IRET from GETGB2: ', IRET_EARLY

        if (IRET_EARLY .ne. 0) THEN

        write(0,*) 'set APCP1 to zero'
        APCP1=0.

        else

        write(0,*) 'size(APCP1): ', size(APCP1)
        write(0,*) 'size(gfld%fld): ', size(gfld%fld)

        APCP1=gfld%fld
        print*,'apcp1=',maxval(apcp1),minval(apcp1)


!       do K=1,29
!       do K=1,200
!       PDS_RAIN_HOLD_EARLY(K)=gfld%ipdtmpl(K)
!       enddo

!       do k=1,21
!         gds_hold(k)=gfld%igdtmpl(k)
!       enddo
      do K=1,29
        PDS_RAIN_HOLD_EARLY(K)=gfld%ipdtmpl(K)
      enddo
      do k=1,5
        drt_rain_hold_early(k)=gfld%idrtmpl(k)
      enddo
      do k=1,21
        gds_rain_hold_early(k)=gfld%igdtmpl(k)
      enddo

        endif

!  SKIP CONVECTIVE PRECIP


!  CONVECTIVE PRECIP
!     J = 0;JPDS = -1;JPDS(3) = IGDNUM
!     JPDS(5) = 063;JPDS(6) = 001
!     JPDS(13) = 1
!     CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,       &
!                K,KPDS,KGDS,MASK,GRID,CAPCP1,IRET,ISTAT)

!  SNOWFALL
      J = 0;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 065;JPDS(6) = 001

!       if (IARW .eq. 0) then

      JPDS(14) = SHR1  ! t-6 or T-3 accumulated snow
      JPDS(15) = FHR1  ! T

      print *,'FHRS ',JPDS(14),JPDS(15),' READ 1st SNOW FILE ', LUGB,LUGI 
        write(0,*) 'SHR1: ', SHR1
!     if (fhr4.gt.0) JPDS(14)=FHR0
!       write(0,*) 'JPDS(14) for snow now: ', JPDS(14)

!       endif

        JIDS=-9999
        JPDTN=1
        JPDTN=8
        JPDT=-9999
        JPDT(2)=13
        JGDTN=-1
        JGDT=-9999

        call getgb2(LUGB,0,0,0,JIDS,JPDTN,JPDT,JGDTN,JGDT, &
                    UNPACK,K,GFLD,IRET)

        if (IRET .ne. 0) THEN

        write(0,*) 'set SNOW1 to zero'
        SNOW1=0.

        else

        SNOW1=gfld%fld
        print*,'snow1=',maxval(snow1),minval(snow1)

!       do K=1,200
!       do K=1,29
!       PDS_SNOW_HOLD_EARLY(K)=gfld%ipdtmpl(K)
!       enddo

!       do k=1,21
!         gds_hold(k)=gfld%igdtmpl(k)
!       enddo

      do K=1,29
        PDS_SNOW_HOLD_EARLY(K)=gfld%ipdtmpl(K)
      enddo
      do k=1,5
        drt_snow_hold_early(k)=gfld%idrtmpl(k)
      enddo
      do k=1,21
        gds_snow_hold_early(k)=gfld%igdtmpl(k)
      enddo

        endif


!=======================================================
!  READ 2nd file
!=======================================================

      print *;print *,'RD 2nd File', FHR2,LUGB2,LUGI2

      ALLOCATE (APCP2(NUMVAL),CAPCP2(NUMVAL),SNOW2(NUMVAL),STAT=kret)
      IF(kret.ne.0)THEN
       WRITE(*,*)'ERROR allocation source location: ',numval
       STOP
      END IF

!     ACCUMULATED PRECIP

        JIDS=-9999
        JPDTN=8
        JPDT=-9999
        JPDT(2)=8
        JGDTN=-1
        JGDT=-9999

        call getgb2(LUGB2,0,0,0,JIDS,JPDTN,JPDT,JGDTN,JGDT, &
                    UNPACK,K,GFLD,IRET)
        APCP2=gfld%fld
        print*,'apcp2=',maxval(apcp2),minval(apcp2)


!       do K=1,200
        do K=1,29
        PDS_RAIN_HOLD(K)=gfld%ipdtmpl(K)
        enddo

!       do k=1,21
!         gds_hold(k)=gfld%igdtmpl(k)
!       enddo

! SKIP ACCUMULATED CONVECTIVE PRECIP

!     ACCUMULATED CONVECTIVE PRECIP
!     J = 0;JPDS = -1;JPDS(3) = IGDNUM
!     JPDS(5) = 063;JPDS(6) = 001
!     JPDS(13) = 1
!     CALL SETVAR(LUGB2,LUGI2,NUMVAL,J,JPDS,JGDS,KF,     &
!                K,KPDS,KGDS,MASK,GRID,CAPCP2,IRET,ISTAT)

!     SNOWFALL
      J = 0;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 065;JPDS(6) = 001
!       if (IARW .eq. 0) then
      JPDS(14) = FHR1
      JPDS(15) = FHR2
!     IF (LSUB) JPDS(14)=FHR3
      IF (LSUB) JPDS(14)=SHR1  ! T-3 Shouldnt need this ??
      print *,'FHRS ',JPDS(14),JPDS(15),' READ 2nd SNOW FILE ', LUGB2,LUGI2
!       endif

        JIDS=-9999
        JPDTN=8
        JPDT=-9999
        JPDT(2)=13
        JGDTN=-1
        JGDT=-9999

        call getgb2(LUGB2,0,0,0,JIDS,JPDTN,JPDT,JGDTN,JGDT, &
                    UNPACK,K,GFLD,IRET)

        SNOW2=gfld%fld
        print*,'snow2=',maxval(snow2),minval(snow2)

!       do K=1,200
        do K=1,29
        PDS_SNOW_HOLD(K)=gfld%ipdtmpl(K)
        enddo

!       do k=1,21
!         gds_hold(k)=gfld%igdtmpl(k)
!       enddo

!     IF (FHR4.GT.0 ) THEN
      IF (LRD3) THEN

!=======================================================
!  READ 3rd file
!  For 12 hr precip summations and DGEX 6 hr calculations
!=======================================================
!      CALL RDHDRS(LUGB3,LUGI3,JPDS,JGDS,              &
!                  IGDNUM,IMAX,JMAX,KMAX,NUMVAL)

        WRITE(FNAME(6:7),FMT='(I2)')LUGB3
        CALL BAOPENR(LUGB3,FNAME,IRETGB)

      ALLOCATE (APCP3(NUMVAL),CAPCP3(NUMVAL),SNOW3(NUMVAL),STAT=kret)
      IF(kret.ne.0)THEN
       WRITE(*,*)'ERROR allocation source location: ',numval
       STOP
      END IF

!     ACCUMULATED PRECIP

        JIDS=-9999
        JPDTN=8
        JPDT=-9999
        JPDT(2)=8
        JGDTN=-1
        JGDT=-9999

        call getgb2(LUGB3,0,0,0,JIDS,JPDTN,JPDT,JGDTN,JGDT, &
                    UNPACK,K,GFLD,IRET)
        APCP3=gfld%fld
        print*,'apcp3=',maxval(apcp3),minval(apcp3)


!       do K=1,200
        do K=1,29
        PDS_RAIN_HOLD(K)=gfld%ipdtmpl(K)
        enddo

!       do k=1,21
!         gds_hold(k)=gfld%igdtmpl(k)
!       enddo

! SKIP ACCUMULATED CONVECTIVE PRECIP

!     ACCUMULATED CONVECTIVE PRECIP
!     J = 0;JPDS = -1;JPDS(3) = IGDNUM
!     JPDS(5) = 063;JPDS(6) = 001
!     JPDS(13) = 1
!     CALL SETVAR(LUGB3,LUGI3,NUMVAL,J,JPDS,JGDS,KF,      &
!                K,KPDS,KGDS,MASK,GRID,CAPCP3,IRET,ISTAT)

!     SNOWFALL
      J = 0 ;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 065;JPDS(6) = 001
!       if (IARW .eq. 0) then
        JPDS(14) = FHR2 ! T-6
        JPDS(15) = FHR3 ! T-3
      print *,'FHR3=',FHR3,' READ 3rd PRECIP FILE ', LUGB3,LUGI3
!       endif

        JIDS=-9999
        JPDTN=8
        JPDT=-9999
        JPDT(2)=13
        JGDTN=-1
        JGDT=-9999

        call getgb2(LUGB3,0,0,0,JIDS,JPDTN,JPDT,JGDTN,JGDT, &
                    UNPACK,K,GFLD,IRET)
        SNOW3=gfld%fld
        print*,'snow3=',maxval(snow3),minval(snow3)


!       do K=1,200
        do K=1,29
        PDS_SNOW_HOLD(K)=gfld%ipdtmpl(K)
        enddo

!       do k=1,21
!         gds_hold(k)=gfld%igdtmpl(k)
!       enddo

        ENDIF
!=======================================================
!  READ 4th file
!  READ INDEX FILE TO GET GRID SPECS for 4th file (FOR 12 hour NAM
!  precip)
!=======================================================
      IF (MK12P .and. LRD4) THEN
        WRITE(FNAME(6:7),FMT='(I2)')LUGB4
        CALL BAOPENR(LUGB4,FNAME,IRETGB)

      ALLOCATE (APCP4(NUMVAL),CAPCP4(NUMVAL),SNOW4(NUMVAL),STAT=kret)
      IF(kret.ne.0)THEN
       WRITE(*,*)'ERROR allocation source location: ',numval
       STOP
      END IF
!     ACCUMULATED PRECIP
      J = 0;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 061;JPDS(6) = 001
      JPDS(13) = 1
      print *;print *,'FHR4=',FHR4,' READ 4th PRECIP FILE ', LUGB4,LUGI4

        JIDS=-9999
        JPDTN=8
        JPDT=-9999
        JPDT(2)=8
        JGDTN=-1
        JGDT=-9999

        call getgb2(LUGB4,0,0,0,JIDS,JPDTN,JPDT,JGDTN,JGDT, &
                    UNPACK,K,GFLD,IRET)
        APCP4=gfld%fld
        print*,'apcp4=',maxval(apcp4),minval(apcp4)


!       do K=1,200
        do K=1,29
        PDS_RAIN_HOLD(K)=gfld%ipdtmpl(K)
        enddo

!       do k=1,21
!         gds_hold(k)=gfld%igdtmpl(k)
!       enddo

! SKIP ACCUMULATED CONVECTIVE PRECIP
!     ACCUMULATED CONVECTIVE PRECIP
!     J = 0;JPDS = -1;JPDS(3) = IGDNUM
!     JPDS(5) = 063;JPDS(6) = 001
!     JPDS(13) = 1
!     CALL SETVAR(LUGB4,LUGI4,NUMVAL,J,JPDS,JGDS,KF,       &
!                K,KPDS,KGDS,MASK,GRID,CAPCP4,IRET,ISTAT)

!     SNOWFALL
      J = 0 ;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 065;JPDS(6) = 001
!       if (IARW .eq. 0) then
        JPDS(14) = FHR3 ! T-3
        JPDS(15) = FHR4 ! T
      print *,JPDS(14),JPDS(15),' READ 4th SNOW FILE ', LUGB4,LUGI4
!       endif

        JIDS=-9999
        JPDTN=8
        JPDT=-9999
        JPDT(2)=13
        JGDTN=-1
        JGDT=-9999

        call getgb2(LUGB4,0,0,0,JIDS,JPDTN,JPDT,JGDTN,JGDT, &
                    UNPACK,K,GFLD,IRET)
        SNOW4=gfld%fld
        print*,'snow4=',maxval(snow4),minval(snow4)



!       do K=1,200
        do K=1,29
        PDS_SNOW_HOLD(K)=gfld%ipdtmpl(K)
        enddo
!       do k=1,21
!         gds_hold(k)=gfld%igdtmpl(k)
!       enddo

      ENDIF 

!=======================================================
!     OUTPUT 3, 6 or 12 hr PRECIP BUCKETS
!=======================================================
      ALLOCATE (APCPOUT(NUMVAL),CAPCPOUT(NUMVAL),SNOWOUT(NUMVAL),STAT=kret)
      allocate (grnd(numval))
      IF(kret.ne.0)THEN
       WRITE(*,*)'ERROR allocation source location: ',numval
       STOP
      END IF

!! LSUB --> 3 h total
!! FHR4 > 0 --> 12 h total
!! nothing --> 6 h total

      print *
      IF (LSUB) THEN
       APCPOUT=APCP2-APCP1
!      CAPCPOUT=CAPCP2-CAPCP1
       SNOWOUT=SNOW2-SNOW1
       KPDS(14)=FHR1
       KPDS(15)=FHR2
       print *, 'OUTPUT 3 HR PRECIP: SUB ', FHR1,FHR2, maxval(apcpout)

      ELSE
        APCPOUT=APCP2+APCP1
!       CAPCPOUT=CAPCP2+CAPCP1
        SNOWOUT=SNOW2+SNOW1

!       6 hr precip 
        IF (MK6P) THEN
          KPDS(14)=FHR3
          IF (LADDSUB) THEN   
            KPDS(15)=FHR1
            APCPOUT=APCP3+(APCP2-APCP1)
!           APCPOUT=APCP1
!           CAPCOUT=CAPC3+(CAPC2-CAPC1)
            SNOWOUT=SNOW3+(SNOW2-SNOW1)
            print *, 'OUTPUT 06 HR PRECIP: ADDSUB',FHR1,FHR2,FHR3,maxval(apcpout)
          ELSE
            KPDS(15)=FHR2
            print *,'OUTPUT 6 HR PRECIP: ADD', FHR1,FHR2,maxval(apcpout)
          ENDIF
        ENDIF

!       12 hr precip
        IF (MK12P) THEN
          KPDS(14)=SHR1
          KPDS(15)=FHR4
          IF (LRD4) THEN
            APCPOUT=APCPOUT+APCP3+APCP4
!           CAPCPOUT=CAPCPOUT+CAPCP3+CAPCP4
            SNOWOUT=SNOWOUT+SNOW3+SNOW4
            print *, ' OUTPUT 12 HR PRECIP: ADD 4 ',FHR1,FHR2,FHR3,FHR4,maxval(apcpout)
          ELSE
            print *, ' OUTPUT 12 HR PRECIP: ADD 2 ',FHR1,FHR2,maxval(apcpout)
          ENDIF
        ENDIF

      ENDIF

! Grib2
        if (IRET_EARLY .ne. 0) then
          gfld%ipdtmpl=PDS_RAIN_HOLD
        else
          gfld%ipdtmpl=PDS_RAIN_HOLD_EARLY
        endif

! need to initialize grid specs
!       gfld%igdtmpl=gds_hold
      gfld%idrtmpl=drt_rain_hold_early
      gfld%igdtmpl=gds_rain_hold_early


!       gfld%ipdtmpl(9)=ihrs1
        gfld%ipdtmpl(9)=0

        do J=16,21
        gfld%ipdtmpl(J)=PDS_RAIN_HOLD(J)
        enddo

        gfld%ipdtmpl(22)=1

! default as a 6 h accumulation?
        write(0,*) 'here FHR3, FHR2: ', FHR3, FHR2
!        if (LSUB) then

!       gfld%ipdtmpl(27)= FHR1-FHR2 ! 3 hr accum
!       gfld%ipdtmpl(9)=FHR2
        gfld%ipdtmpl(27)= FHR2-FHR1 ! 3 hr accum
        gfld%ipdtmpl(9)=FHR1


!! use of (27) here looks wrong!

!        write(0,*) 'gfld%ipdtmpl(27) bef: ', &
!                    gfld%ipdtmpl(27)
!        write(0,*) 'gfld%ipdtmpl(9) bef: ', &
!                    gfld%ipdtmpl(9)

!        gfld%ipdtmpl(27)=3
!        gfld%ipdtmpl(9)=FHR1

       write(0,*) 'gfld%ipdtmpl(27) aft: ', &
                   gfld%ipdtmpl(27)
       write(0,*) 'gfld%ipdtmpl(9) aft: ', &
                   gfld%ipdtmpl(9)

!        endif

        IF (MK6P .and. .not.(LSUB)) THEN
        gfld%ipdtmpl(27)=6
!       gfld%ipdtmpl(9)=FHR3
!       gfld%ipdtmpl(9)=SHR1
!       gfld%ipdtmpl(9)=FHR1
        if(laddsub)then
          gfld%ipdtmpl(9)=FHR1
        else
          gfld%ipdtmpl(9)=SHR1
        endif

       write(0,*) 'gfld%ipdtmpl(27) aft: ', &
                   gfld%ipdtmpl(27)
       write(0,*) 'gfld%ipdtmpl(9) aft: ', &
                   gfld%ipdtmpl(9)
        ENDIF
! Grib2
 
!       12 hr precip
        IF (MK12P) THEN
!         KPDS(14)=SHR1
!         KPDS(15)=FHR4
!         IF (LRD4) THEN
!           APCPOUT=APCPOUT+APCP3+APCP4
!           CAPCPOUT=CAPCPOUT+CAPCP3+CAPCP4
!           SNOWOUT=SNOWOUT+SNOW3+SNOW4
!           print *, ' OUTPUT 12 HR PRECIP: ADD 4 ',FHR1,FHR2,FHR3,FHR4,maxval(apcpout)
!         ELSE
!           print *, ' OUTPUT 12 HR PRECIP: ADD 2 ',FHR1,FHR2,maxval(apcpout)
!         ENDIF
        gfld%ipdtmpl(27)=12
        gfld%ipdtmpl(9)=SHR1

        ENDIF

!     ENDIF

!  compute nbits

! force binary scaling of adequate precision
        gfld%idrtmpl(2)=-4

        call getbit(0,abs(gfld%idrtmpl(2)), &
          gfld%idrtmpl(3),numval,0,apcpout, &
          grnd,gmin,gmax,nbit)

      gfld%idrtmpl(4)=nbit

! end nbits mod

      KPDS(5)=61
      print *, 'writing precip', KPDS(5),KPDS(14),KPDS(15),LUGB5,MINVAL(APCPOUT),MAXVAL(APCPOUT)
      WRITE(FNAME(6:7),FMT='(I2)')LUGB5
      print*,'fname,lugb5=',fname,lugb5
      CALL BAOPEN(LUGB5,FNAME,IRETGB)
      print*,'opened file'
!     CALL PUTGB(LUGB5,NUMVAL,KPDS,KGDS,MASK,APCPOUT,IRET)
!     print*,'apcpout=',apcpout
        gfld%ipdtmpl(2)=8
        gfld%fld=APCPOUT
!       gfld%fld=SNOWOUT
      print*,'calling putgb2'
      call putgb2(LUGB5,GFLD,IRET)
      print*,'calling baclose'
      CALL BACLOSE(LUGB5,IRET)

! SKIP CONVECTIVE ACCUMULATED PRECIP
!     KPDS(5)=63
!     print *, 'writing CAPCP', KPDS(5),KPDS(14),KPDS(15),LUGB6 , MAXVAL(CAPCPOUT)
!     WRITE(FNAME(6:7),FMT='(I2)')LUGB6
!     CALL BAOPEN(LUGB6,FNAME,IRET)
!     CALL PUTGB(LUGB6,NUMVAL,KPDS,KGDS,MASK,CAPCPOUT,IRET)
!     CALL BACLOSE(LUGB6,IRET)

!  compute nbits

! force binary scaling of adequate precision
        gfld%idrtmpl(2)=-4

        call getbit(0,abs(gfld%idrtmpl(2)), &
          gfld%idrtmpl(3),numval,0,snowout, &
          grnd,gmin,gmax,nbit)

      gfld%idrtmpl(4)=nbit

! end nbits mod
      KPDS(5)=65
      print *, 'writing SNOW', KPDS(5),KPDS(14),KPDS(15),LUGB7,MINVAL(SNOWOUT),MAXVAL(SNOWOUT)
      WRITE(FNAME(6:7),FMT='(I2)')LUGB7
      CALL BAOPEN(LUGB7,FNAME,IRET)
!     CALL PUTGB(LUGB7,NUMVAL,KPDS,KGDS,MASK,SNOWOUT,IRET)
      gfld%ipdtmpl(2)=13
      gfld%fld=SNOWOUT
      call putgb2(LUGB7,GFLD,IRET)
      CALL BACLOSE(LUGB7,IRET)

      STOP
      END

      SUBROUTINE SETVAR(LUB,LUI,NUMV,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,VARB,IRET,ISTAT)
!============================================================================
!     This Routine reads in a grib field and initializes a 2-D variable
!     Requested from w3lib GETGRB routine
!     10-2012   Jeff McQueen
!     NOTE: ONLY WORKS for REAL Type Variables
!============================================================================
      INTEGER,  INTENT(IN)     :: lub,lui,numv             ! unit numbers
      INTEGER,  INTENT(INOUT)  :: jpds(200),jgds(200)      ! grib parms
      INTEGER,  INTENT(OUT)    :: k,kf,kpds(200),kgds(200) ! grid info
      INTEGER,  INTENT(OUT)    :: iret,istat               ! grid info
      LOGICAL,  INTENT(INOUT) :: MASK(:)                  ! L/S mask 
      REAL,     INTENT(INOUT)  :: GRID(:)                  ! grib data
      REAL,     INTENT(OUT)    :: VARB(:)                  ! output varb

!     Get GRIB Variable
      CALL GETGB(LUB,LUI,NUMV,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        DO KK = 1, NUMV
          VARB(KK) = GRID(KK)
        ENDDO
        WRITE(6,100) JPDS(5),JPDS(6),JPDS(7),J,MAXVAL(VARB),KF,K
 100    FORMAT('VARB UNPACKED ', 4I7,G12.4,I8, ' RECORD',I5)
      ELSE
        WRITE(6,*)'===================================================='
        WRITE(6,*)'SETVAR : COULD NOT UNPACK VARB',JPDS(5),JPDS(6)
        WRITE(6,*)' J ',J,' GRID ',JPDS(3),'GETGB RETURN CODE',IRET
        WRITE(6,*)'UNIT', LUB,LUI,'NUMVAL ', NUMV,KF,' RECORD',K
        WRITE(6,*)'===================================================='
        print *, 'JPDS', JPDS(1:25)
        print *, 'JGDS', JGDS(1:25)
        ISTAT = IRET

! GUAM only has accumulated precip, not convective precip or snow
!FOR GUAM        STOP 'UNIFPRECIP ABORT:   VARB NOT UNPACKED'
      ENDIF

      RETURN
      END

      SUBROUTINE RDHDRS(LUB,LUI,JPDS,JGDS,IGDN,IMAX,JMAX,KMAX,NUMV)
!=============================================================================
!     This Routine Reads GRIB index file and returns its contents
!     (GETGI)
!     Also reads GRIB index and grib file headers to
!     find a GRIB message and unpack pds/gds parameters (GETGB1S)
!
!     10-2012  Jeff McQueen
!==============================================================================
      INTEGER,    INTENT(IN)     :: lub,lui                  ! unit numbers
      INTEGER,    INTENT(INOUT)  :: jpds(200),jgds(200)      ! grib parms
      INTEGER,    INTENT(OUT)    :: igdn,imax,jmax,kmax,numv ! grid size

      INTEGER,PARAMETER :: MBUF = 2000000 !Character length of bufr varb
      CHARACTER*80 FNAME
      CHARACTER CBUF(MBUF)
      INTEGER KPDS(200),KGDS(200)
      INTEGER JENS(200),KENS(200)

!jtm  Input Filename prefix on WCOSS
      FNAME='fort.  '

      IRGI = 1
      IRGS = 1
      KMAX = 0
      JR=0
      KSKIP = 0

      WRITE(FNAME(6:7),FMT='(I2)')LUB
      CALL BAOPEN(LUB,FNAME,IRETGB)
      WRITE(FNAME(6:7),FMT='(I2)')LUI
      CALL BAOPEN(LUI,FNAME,IRETGI)

      CALL GETGI(LUI,KSKIP,MBUF,CBUF,NLEN,NNUM,IRGI)

      write(6,*)' IRET FROM GETGI ',IRGI,' UNIT ',LUB,LUI,' NLEN',NLEN
      IF(IRGI .NE. 0) THEN
        WRITE(6,*)' PROBLEMS READING GRIB INDEX FILE SO ABORT',IRGI
        ISTAT = IRGI
        STOP 'RDHDRS ABORT GETGI'
      ENDIF

       DO K = 1, NNUM
        JR = K - 1
        JPDS = -1
        JGDS = -1
        CALL GETGB1S(CBUF,NLEN,NNUM,JR,JPDS,JGDS,JENS,    &
                     KR,KPDS,KGDS,KENS,LSKIP,LGRIB,IRGS)
        IF(IRGI .NE. 0) THEN
          WRITE(6,*)' PROBLEMS ON 1ST READ OF GRIB FILE SO ABORT',IRGS
          ISTAT = IRGS
          STOP 'RDHDRS ABORT: GETBS1S'
        ENDIF
        IGDN = KPDS(3)
        IMAX = KGDS(2)
        JMAX = KGDS(3)
        NUMV = IMAX*JMAX
        KMAX = 0    ! HARDWIRED FOR PRECIP
      ENDDO

      WRITE(6,280) IGDN,JPDS(4),JPDS(5),IMAX,JMAX,KMAX
  280 FORMAT(' IGDN, IMAX,JMAX,KMAX ',6I5)
      RETURN 
      END
