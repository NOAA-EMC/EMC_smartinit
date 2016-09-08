   SUBROUTINE GETGRIB(ISNOW,IZR,IIP,IRAIN,VEG,WETFRZ,  &
   P03M,P06M,P12M,SN03,SN06,S3REF01,S3REF10,S3REF50,S6REF01,  &
   S6REF10,S6REF50,S12REF01,S12REF10,S12REF50, THOLD,DHOLD,GDIN,VALIDPT, &
   GFLD_S,GFLD8_S,HAVESREF)

    use grddef
    use aset3d
    use aset2d
    use rdgrib
    use constants
    USE GRIB_MOD
    USE pdstemplates

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    GETGRIB    CREATES NDFD FILES 
!   PRGRMMR: MANIKIN           ORG: W/NP22     DATE: 06-09-14
!
! ABSTRACT:
!   READS GRIB FILE for smartinit downscaling

!   Precip read are conditioned on whether on or off cycle run
!   ON-CYCLE GRIB FILES have 6,12 hour precip buckets
!   OFF-CYCLE GRIB FILES have 3 hr precip buckets

! PROGRAM HISTORY LOG:
!   06-09-14  G MANIKIN  - ADAPT CODE TO NAM 
!   12-10-01  J.MCQUEEN  - Reduced code thru use of rdhdrs,setvar
!   subrountines
!   12-10-01             - Combined on and off-cycle reads into getgrib
!   15-05-27  A.GIBBS    - Changes to process GRIB2 (following Matt Pyle's
!                          changes to HIRESW Smartinit)
!   16-06-24  A.GIBBS    - Changes to process GRIB2 added to latest version of
!                          smartinit

! USAGE:    CALL SMARTINIT 
!   INPUT ARGUMENT LIST:

!   OUTPUT ARGUMENT LIST:
!     NONE

!   OUTPUT FILES:
!     NONE

      TYPE (GINFO) :: GDIN
      INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)
      INTEGER YEAR,MON,DAY,IHR,DATE,IFHR,HAVESREF
      INTEGER:: NUMVAL, IMAX, JMAX, KMAX, NUMLEV

      INTEGER :: LUB,LUI,J,JDISC,JPDTN,JGDTN
      INTEGER,DIMENSION(:) :: JIDS(200),JPDT(200),JGDT(200)
      LOGICAL :: UNPACK
      INTEGER :: K,IRET, IGDNUM, IGDNUM2, IGDNUM3, IGDNUMSN3, NUMVAL2
      INTEGER :: NUMVAL3, NUMVALSN3, IGDNUM5
      INTEGER :: IGDNUM6, NUMVAL6, IGDNUMSN6, NUMVALSN6
      INTEGER :: IGDNUM12, NUMVAL12, IGDNUMT, NUMVALT, LUGTI, IT, KRET
      INTEGER :: ITOT, KK, NUMVP, NUMVS, IFHR4, IFHR12, KT, LUGTA, LUGTB
      INTEGER :: IIH, LL, M, N, I, ISTAT, KF, ISSREF
      TYPE(GRIBFIELD):: GFLD, GFLD8, GFLD_S, GFLD8_S

      PARAMETER(MBUF=2000000)
      CHARACTER CBUF(MBUF)
      CHARACTER*80 FNAME
      CHARACTER*4 DUM1, REGION, CORE
      LOGICAL*1 LCYCON,LHR3,LHR6,LHR12,LFULL,LANL,LLIMITED, LHIRESW
      LOGICAL LNEST   ! for nests
      INTEGER JENS(200),KENS(200),CYC

!   REAL,        ALLOCATABLE   :: GRID(:)
!   LOGICAL*1,   ALLOCATABLE   :: MASK(:)
!-----------------------------------------------------------------------------------------

!  TYPE(ISET), INTENT(INOUT) :: iprcp
   INTEGER, INTENT(INOUT) :: ISNOW(:,:),IZR(:,:),IIP(:,:),IRAIN(:,:)
   REAL, allocatable,dimension(:,:) :: RTYPE

!  TYPE PCPSET, INTENT(INOUT) :: prcp
   REAL,    INTENT(INOUT) :: P03M(:,:),P06M(:,:),P12M(:,:),SN03(:,:),SN06(:,:)
   REAL,    INTENT(INOUT) :: WETFRZ(:,:)
   REAL,    INTENT(INOUT) :: THOLD(:,:,:),DHOLD(:,:,:)

   REAL,    INTENT(INOUT) :: VEG(:,:)

!  TYPE POPSET, INTENT(INOUT) :: pop
   REAL,    INTENT(INOUT) :: S3REF01(:,:),S3REF10(:,:),S3REF50(:,:)
   REAL,    INTENT(INOUT) :: S6REF01(:,:),S6REF10(:,:),S6REF50(:,:)
   REAL,    INTENT(INOUT) :: S12REF01(:,:),S12REF10(:,:),S12REF50(:,:)

   LOGICAL, INTENT(INOUT) :: VALIDPT(:,:) 

   INTEGER :: IHROFF, FHR, LUGB, LUGI, LUGB2, LUGI2, LUGP3, LUGP3I
   INTEGER :: LUGS3, LUGS3I, LUGP6, LUGP6I, LUGS6, LUGS6I
   INTEGER :: LUGP12, LUGP12i, LUGT, LUGT1, LUGT2, LUGT3, LUGT4, LUGT5
   INTEGER :: LUGT1I, LUGT2I, LUGT3I, LUGT4I, LUGT5I

   LOGICAL :: LHR9

!-----------------------------------------------------------------------------------------

!    09-2012 JTM : Modified I/O for WCOSS fort. file name nomenclature
!                  ENVVAR not needed
!                  Introduced RDHDRS and SETVAR routines to eliminate redundancies
!    10-2012 JTM : Added dynamic allocation for arrays
!    11-2012 JTM : Added options for hi, ak, pr domains
!    12-2012 JTM : Added options for conusnest (validpt sets)

      IHROFF=0;LHR12=.FALSE.; LHR6=.FALSE.; LHR3=.FALSE.
      LFULL=.FALSE.;LANL=.FALSE.;LLIMITED=.FALSE.;LCYCON=.FALSE.

      FHR=GDIN%FHR;IFHR=FHR;CYC=GDIN%CYC;LNEST=GDIN%LNEST;INHRFRQ=GDIN%INHRFRQ
      REGION=GDIN%REGION;IFHRSTR=GDIN%IFHRSTR
      LHR9=.false.
      CORE=GDIN%CORE        !arw or nmmb for hiresw runs or dgex
      LHIRESW=GDIN%LHIRESW  !For hiresw runs
      print *, 'CORE',CORE, 'REGION=', GDIN%REGION, GDIN%IFHRSTR
      IF (IFHR.EQ.IFHRSTR) THEN
        LANL=.TRUE.
      ELSE
        IF (MOD(IFHR,3).EQ.0) THEN 
          LFULL=.TRUE.
        ELSE
          LLIMITED=.TRUE.
        ENDIF
      ENDIF

        IGDNUMT=0

        write(0,*) 'inside GETGRIB2'
        write(0,*) 'LNEST: ', LNEST
       
!    SET LOGICALS FOR when to read precip or max/min from special files

!                    NON-Nest(on)      NON-Nest(off)     NESTS
!----------------------------------------------------------------
!    F3,15, 27            X              3 hr prcp        X
!    other mod(3h)    3 hr prcp          3 hr prcp        X
!    mod(6 fhrs)         X              3,6 hr prcp      6 hr prcp
!    mod(12 fhrs)     3,6 hr prcp        3,6 hr prcp      6,12 hr prcp
!    MAX/MIN 12 hrs   prev 11 hrs        prev 11 hrs
!----------------------------------------------------------------

      IF (CYC.EQ.12.OR.CYC.EQ.00) LCYCON=.TRUE.

!     Set full, sref and special precip file unit numbers
      LUGB=11; LUGI=12   !DEFAULT MODEL FULL GRIB FILE UNITS
      LUGB2=13; LUGI2=14 !DEFAULT SREF POP GRIB FILE UNITS

      if (trim(CORE) .EQ. 'dgx'.and. lanl) THEN
        LUGP3=11;LUGP3i=12
        LUGS3=11;LUGS3i=12
      endif

      if (.not.lanl) then
       LUGP3=11;  LUGP3i=12
       LUGS3=11;  LUGS3i=12
       LUGP6=11;  LUGP6i=12
       LUGS6=11;  LUGS6i=12
       LUGP12=11; LUGP12i=12

       IF(MOD(IFHR,3).EQ.0) LHR3=.TRUE.  
       IF((IFHR-IFHRSTR).GE.6.and.MOD(IFHR,6).EQ.0) LHR6=.TRUE.
       IF(LCYCON) THEN
         IF(MOD(IFHR,12).EQ.9)  LHR9=.TRUE.
         IF(MOD((IFHR-IFHRSTR),12).EQ.0) LHR12=.TRUE.
       ELSE
         IF((IFHR-IFHRSTR).GT.6 .AND. MOD(IFHR-6,12).EQ.0) LHR12=.TRUE.
       ENDIF
      
!     Set precip unit numbers for nests
       IF (lnest) THEN
!        DGEX std file has 3 or  6 hr precip only 
         if (trim(CORE) .EQ. 'dgx'.and. LHR6) THEN
           LUGP6=11;LUGP6i=12
           LUGS6=11;LUGS6i=12
           LUGP3=15;  LUGP3i=16
           LUGS3=17;  LUGS3i=18
         elseif (trim(CORE) .EQ. 'GFS' .and. LHR6) THEN
           LUGP3=15; LUGP3i=16
           LUGS3=17; LUGS3i=18
           LUGP6=19; LUGP6i=20
           LUGS6=21; LUGS6i=22
           IF (trim(CORE) .EQ. 'GFS' .and. LHR12) THEN
             LUGP12=23; LUGP12i=24
           ENDIF
         else
           LUGP6=15;LUGP6i=16
           LUGS6=17;LUGS6i=18
         endif
         IF (trim(CORE).NE.'GFS') THEN
           LUGP12=19;LUGP12i=20
         ENDIF
         LHR9=.FALSE.   ! nests have 3 hour precip in std parent grid (01-28-13, JTM)
       else
         IF(LCYCON) THEN 
           IF(MOD(IFHR,12).NE.3) THEN  !all 3 hrs except 3,15,27,39,51,63,75
             LUGP3=15; LUGP3i=16  
             LUGS3=17; LUGS3i=18   
           ENDIF
           IF(LHR12) THEN
             LUGP6=17; LUGP6i=18
             LUGS3=19; LUGS3i=20 
             LUGS6=21; LUGS6i=22
           ENDIF
         ELSE
           LHR9=.FALSE.    ! off-cycle have 3 hr precip in std parent grid
           IF (LHR6) THEN    
             LUGP6=15; LUGP6i=16   !SPECIAL PRECIP FILE
             LUGS6=17; LUGS6i=18
           ENDIF
           IF (LHR12) LUGP12=19;LUGP12i=20   
         ENDIF
        endif  !lnest
      endif  !lanl

      print *, 'IFHR',IFHR,'LHR3',LHR3,'LHR6',LHR6,'LHR12',LHR12
      P03M=0.0; P06M=0.0; S03M=0.0; S06M=0.0; P12M=0.0

!     SET MAX/MIN FILE UNIT NUMBERS
!     FOR 12-hr on-cycle TIMES, WE NEED 3 AND 6-HR BUCKETS AND MAX/MIN TEMP
!     DATA FOR THE PREVIOUS 11 HOURS
      IF(LHR12) THEN
       LUGT1=23
       IF (.not.LCYCON .or. lnest) LUGT1=21
       IF (trim(CORE) .EQ. 'GFS') LUGT1=25
       LUGT2=LUGT1+1
       LUGT3=LUGT1+2
       LUGT4=LUGT1+3
       LUGT5=LUGT1+4
       LUGT1I=LUGT1+5
       LUGT2I=LUGT1+6
       LUGT3I=LUGT1+7
       LUGT4I=LUGT1+8
       LUGT5I=LUGT1+9
        print *,'====================================================='
        print *, 'Read  11 hrs of MAX,MIN TEMP', IFHR,lugt1,lugt2,lugt3
        print *, 'Read  3 hr precip from unit',lugp3,lugs3
        print *, 'Read  6 hr precip from unit',lugp6,lugs6
        print *, 'Read  12 hr precip from unit',lugp12
        print *,'====================================================='

      ELSE IF(LHR6.OR.LHR9) THEN
!      However Off-Hour cycle runs do not have 6 hour buckets
         LUGT1=19; LUGT2=20; LUGT1I=21; LUGT2I=22
         IF(trim(CORE) .EQ. 'GFS') THEN
           LUGT1=23;LUGT2=24; LUGT1I=25; LUGT2I=26
         ENDIF
       print *,'======================================================='
       print *, 'Read previous 2 hrs of  MAX,MIN TEMP', IFHR, lugt1,lugt2
       print *, 'Read  3 hr precip from unit',lugp3,lugs3
       if(lhr6) print *,'Read 6 hr precip from unit',lugp6,lugs6
       print *,'======================================================='

      ELSE IF(LHR3) THEN
        LUGT1=15;LUGT2=16; LUGT1I=17; LUGT2I=18
       print *,'================================================================='
       print *, 'Read previous 2 hours of MAX,MIN TEMP ', IFHR,lugt1,lugt2
       print *, 'Read 3 hr prcp from std grid ',lugp3,lugs3
       print *,'================================================================='
      ELSE

!      IN-BETWEEN HOURS DON'T NEED ANYTHING FANCY
      print *,'====================================================='
       print *, 'IN-Between HOURS, small change ', IFHR
       print *,'====================================================='
      ENDIF

      OPEN(49,file='DATE',form='formatted')
      READ(49,200) DUM1,GDIN%DATE
      DATE=GDIN%DATE
      CLOSE(49)
 200  FORMAT(A4,2X,I10)
      year=int(date/1000000)
      mon=int(int(mod(date,1000000)/100)/100)
      day=int(mod(date,10000)/100)
      ihr=mod(date,100)
      print *, 'date ', DATE,YEAR,MON,DAY,IHR 

!==========================================================
!     READ INDEX FILE TO GET GRID SPECS
!==========================================================
!     CALL RDHDRS(LUGB,LUGI,IGDNUM,GDIN,NUMVAL)
        write(0,*) 'here c - call RDHDRS'
      CALL RDHDRS_g2(LUGB,LUGI,IGDNUM,GDIN,NUMVAL)
      IMAX=GDIN%IMAX;JMAX=GDIN%JMAX;KMAX=GDIN%KMAX
      NUMLEV=GDIN%KMAX
      ITOT=IMAX*JMAX
      print *,'imax,jmax,kmax,numlev,itot,core,lhiresw'
      print *,gdin%imax,jmax,kmax,numlev,itot,core,lhiresw
        write(0,*) 'here d - past RDHDRS'
        write(0,*) 'see NUMLEV: ', NUMLEV

      if (lfull) then
      if (HAVESREF .eq. 1)then
      print *, ' READING SREF HDRS',LUGB2,LUGI2
!     CALL RDHDRS(LUGB2,LUGI2,IGDNUM2,GDIN,NUMVAL2)
      CALL RDHDRS_g2(LUGB2,LUGI2,IGDNUM2,GDIN,NUMVAL2)

        write(0,*) 'here e - past RDHDRS'

      endif ! HAVESREF

! GSM  READ 3-HR PRECIP AND SNOW FILES WHICH ARE NEEDED
!      IF NOT A 3-HR ACCUMULATION TIME (F15,F27,F39...) 
!      OR AN "OFF-TIME" (F13,F14,F16....)

      print *, 'READ 3-hr precip HDRS from Unit ', LUGP3,LUGP3I
!     CALL RDHDRS(LUGP3,LUGP3I,IGDNUM3,GDIN,NUMVAL3)
      CALL RDHDRS_g2(LUGP3,LUGP3I,IGDNUM3,GDIN,NUMVAL3)
        write(0,*) 'here f - past RDHDRS'
      print *, 'READ 3-hr snow HDRS from Unit ', LUGS3,LUGS3I
!     CALL RDHDRS(LUGS3,LUGS3I,IGDNUMSN3,GDIN,NUMVALSN3)
      CALL RDHDRS_g2(LUGS3,LUGS3I,IGDNUMSN3,GDIN,NUMVALSN3)
        write(0,*) 'here g - past RDHDRS'
      IGDNUM5=IGDNUMSN3

!     READ 6-HR PRECIP/SNOW FILES AT F12,F24,F36.....
!     OR 12-hr PRECIP FOR OFF-CYCLE RUNS
      IF (LHR6.OR.LHR9.OR.LHR12) THEN
!       print *, 'READING 6 hr precip HDR from Unit ', LUGP6,LUG6PI
        print *, 'READING 6 hr precip HDR from Unit ', LUGP6,LUGP6I
!       CALL RDHDRS(LUGP6,LUGP6I,IGDNUM6,GDIN,NUMVAL6)
        CALL RDHDRS_g2(LUGP6,LUGP6I,IGDNUM6,GDIN,NUMVAL6)
        write(0,*) 'here gb - '
        print *, 'READING 6 hr SNOW HDR from Unit ', LUGS6,LUGS6I
!       CALL RDHDRS(LUGS6,LUGS6I,IGDNUMSN6,GDIN,NUMVALSN6)
        CALL RDHDRS_g2(LUGS6,LUGS6I,IGDNUMSN6,GDIN,NUMVALSN6)
        write(0,*) 'here gc - '
        IF(LHR12) THEN
          print *, 'READING 12 hr precip HDR from Unit ', LUGP12,LUGP12I
!         CALL RDHDRS(LUGP12,LUGP12I,IGDNUM12,GDIN,NUMVAL12)
        write(0,*) 'here gd - '
          CALL RDHDRS_g2(LUGP12,LUGP12I,IGDNUM12,GDIN,NUMVAL12)
        write(0,*) 'here ge - '
        ENDIF
      ENDIF
        write(0,*) 'here h '

!==================================================================
! GSM  READ TEMPERATURE HDR FILES FOR 12-HR MIN/MAX
!      READ INDEX FILE TO GET GRID SPECS
!==================================================================
!     GET GRID NUMBER FROM PDS AND PROCESS GRIB FILE
!     NOTE: WE'LL ASSUME THE GRID NUMBER IS THE SAME FOR
!     ALL OF THESE MIN/MAX FILES AND NOT DO THIS FOR EACH

      print *, "Reading min/max Temp HDR from UNIT:",LUGT1, LUGT1I
!     CALL RDHDRS(LUGT1,LUGT1I,IGDNUMT,GDIN,NUMVALT)
      CALL RDHDRS_g2(LUGT1,LUGT1I,IGDNUMT,GDIN,NUMVALT)
        write(0,*) 'past RDHDRS_g2(aa) call'

!     Fill the max/min T/Td holders with 0's to
!      1) account for this array at times other than f12,24,36...
!      2) temporarily fill the 12th time slot, since we have only 11 here
      THOLD=0.
      DHOLD=0.

      print *, "Reading min/max Temp HDR from UNIT:",LUGT2, LUGT2I, NUMVALT
!     CALL RDHDRS(LUGT2,LUGT2I,IGDNUMT,GDIN,NUMVALT)
      CALL RDHDRS_g2(LUGT2,LUGT2I,IGDNUMT,GDIN,NUMVALT)
        write(0,*) 'past RDHDRS_g2(a) call'

      IF (LHR12) THEN
        LUGT=LUGT3    
        LUGTI=LUGT3I   
        DO  IT=3,5
          print *, "Reading min/max Temp  UNIT:",LUGT, LUGTI
!         CALL RDHDRS(LUGT,LUGTI,IGDNUMT,GDIN,NUMVALT)
          CALL RDHDRS_g2(LUGT,LUGTI,IGDNUMT,GDIN,NUMVALT)
        write(0,*) 'past RDHDRS_g2(b) call'
          LUGT=LUGT+1
          LUGTI=LUGTI+1
        ENDDO 
      ENDIF
      endif !LFULL

!  get sfc height 
      ALLOCATE (GRID(ITOT),MASK(ITOT),STAT=kret)
      print *,'GRID ALLOCATED',ITOT,' kret',kret
!     J=0;JPDS=-1;JGDS=-1;JPDS(3) = IGDNUM
!     JPDS(5) = 007
!     JPDS(6) = 001
!     CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,ZSFC,IRET,ISTAT)
        JIDS=-9999
        JPDTN=-1
        JPDT=-9999
        JGDTN=-1
        JGDT=-9999

       JPDT(2) = 005
       JPDT(10) = 1
       JPDTN   = 0
       JDISC = 0

        write(0,*) 'call SETVAR_g2 for ZSFC'
        write(0,*) 'NUMVAL into SETVAR_g2: ', NUMVAL

! is it SREF data?
        ISSREF=0

        J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K, &
                     KPDS,KGDS,MASK,GRID,ZSFC,GFLD_S,ISSREF,IRET,ISTAT)

        write(0,*) 'GFLD_S%ibmap: ', GFLD_S%ibmap

      WHERE (ZSFC < 0.0) ZSFC=0.0

        write(0,*) 'minval(zsfc),maxval(zsfc): ', minval(zsfc),maxval(zsfc)

! get surface pressure
!     JPDS(5) = 001
!     JPDS(6) = 001
!     CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,PSFC,IRET,ISTAT)

       JPDT(1) = 003
       JPDT(2) = 000
       JPDT(10) = 1
       JPDTN   = 0

        write(0,*) 'call for pressure'
        J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,PSFC,GFLD,ISSREF,IRET,ISTAT)

        if (IRET .eq. 0) then
        write(0,*) 'minval(psfc),maxval(psfc); ', minval(psfc),maxval(psfc)
        endif

! get 4 INTEGER precip types 
      if (lfull) then
!     J=0;JPDS=-1;JPDS(3)=IGDNUM 
!     JPDS(5) = 143 
!     JPDS(6) = 001

! snow

        JIDS=-9999
        JPDT=-9999
        JGDTN=-1
        JGDT=-9999
        JDISC=0

        JPDTN=0
       JPDT(1) = 001
       JPDT(2) = 195 ! 036
       JPDT(10) = 1
        UNPACK=.true.
        J=0

        allocate(RTYPE(IMAX,JMAX))
!      CALL GETGB2(LUGB,LUGI,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDTN,JGDT, &
!                  UNPACK,K,GFLD,IRET)

        J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,RTYPE,GFLD,ISSREF,IRET,ISTAT)

!     Get INTEGER GRIB Variable  
!     CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,IRET)

        write(0,*) 'IRET from snow cat: ', IRET

        do J=1,JMAX
        do I=1,IMAX
        ISNOW(I,J)=int(RTYPE(I,J))
        enddo
        enddo

!     IF(IRET.EQ.0) THEN
!       DO KK = 1, ITOT
!         IF(MOD(KK,IMAX).EQ.0) THEN
!           M=IMAX
!           N=INT(KK/IMAX)
!         ELSE
!           M=MOD(KK,IMAX)
!           N=INT(KK/IMAX) + 1
!         ENDIF
!         ISNOW(M,N) = GRID(KK)
!       ENDDO
!       WRITE(6,*) JPDS(5),JPDS(6),JPDS(7),J,KF,K
!     ELSE
!      WRITE(6,*)'COULD NOT UNPACK VARB',J,JPDS(3),JPDS(5),IRET
!      ISTAT = IRET
!     ENDIF

! ice pellets
       JPDT(1) = 001
       JPDT(2) = 194 ! 035
       JPDT(10) = 1

        write(0,*) 'to ice pellets'

        J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,RTYPE,GFLD,ISSREF,IRET,ISTAT)

        write(0,*) 'IRET from snow cat: ', IRET

        do J=1,JMAX
        do I=1,IMAX
        IIP(I,J)=int(RTYPE(I,J))
        enddo
        enddo

!      CALL GETGB2(LUGB,LUGI,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDTN,JGDT, &
!                  UNPACK,K,GFLD,IRET)
!     JPDS=-1;J=0
!     JPDS(5) = 142
!     JPDS(6) = 001
!     CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,IRET)
!     IF(IRET.EQ.0) THEN
!       DO KK = 1, ITOT
!         IF(MOD(KK,IMAX).EQ.0) THEN
!           M=IMAX
!           N=INT(KK/IMAX)
!         ELSE
!           M=MOD(KK,IMAX)
!           N=INT(KK/IMAX) + 1
!         ENDIF
!         IIP(M,N) = GRID(KK)
!       ENDDO
!       WRITE(6,*) JPDS(5),JPDS(6),JPDS(7),J,KF,K
!     ELSE
!      WRITE(6,*)'COULD NOT UNPACK VARB',J,JPDS(3),JPDS(5),IRET
!      ISTAT = IRET
!     ENDIF

! frz rain
       JPDT(1) = 001
       JPDT(2) = 193 ! 034
       JPDT(10) = 1

!      CALL GETGB2(LUGB,LUGI,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDTN,JGDT, &
!                  UNPACK,K,GFLD,IRET)
!     JPDS=-1;J=0
!     JPDS(5) = 141
!     JPDS(6) = 001
!     CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K, KPDS,KGDS,MASK,GRID,IRET)
!     IF(IRET.EQ.0) THEN
!       DO KK = 1, ITOT
!         IF(MOD(KK,IMAX).EQ.0) THEN
!           M=IMAX
!           N=INT(KK/IMAX)
!         ELSE
!           M=MOD(KK,IMAX)
!           N=INT(KK/IMAX) + 1
!         ENDIF
!         IZR(M,N) = GRID(KK)
!       ENDDO
!       WRITE(6,*) JPDS(5),JPDS(6),JPDS(7),J,KF,K
!     ELSE
!      WRITE(6,*)'COULD NOT UNPACK VARB',J,JPDS(3),JPDS(5),IRET
!      ISTAT = IRET
!     ENDIF

        J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,RTYPE,GFLD,ISSREF,IRET,ISTAT)

        write(0,*) 'IRET from freezing rain cat: ', IRET

        do J=1,JMAX
        do I=1,IMAX
        IZR(I,J)=int(RTYPE(I,J))
        enddo
        enddo

! rain
       JPDT(1) = 001
       JPDT(2) = 192 ! 034
       JPDT(10) = 1

!      CALL GETGB2(LUGB,LUGI,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDTN,JGDT, &
!                  UNPACK,K,GFLD,IRET)
!     JPDS=-1;J=0
!     JPDS(5) = 140
!     JPDS(6) = 001
!     Get INTEGER GRIB Variable  
!     CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,IRET)
!     IF(IRET.EQ.0) THEN
!       DO KK = 1, ITOT
!         IF(MOD(KK,IMAX).EQ.0) THEN
!           M=IMAX
!           N=INT(KK/IMAX)
!         ELSE
!           M=MOD(KK,IMAX)
!           N=INT(KK/IMAX) + 1
!         ENDIF
!         IRAIN(M,N) = GRID(KK)
!       ENDDO
!       WRITE(6,*) JPDS(5),JPDS(6),JPDS(7),J,KF,K
!     ELSE
!      WRITE(6,*)'COULD NOT UNPACK VARB',J,JPDS(3),JPDS(5),IRET
!      ISTAT = IRET
!     ENDIF

        J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,RTYPE,GFLD,ISSREF,IRET,ISTAT)

        write(0,*) 'IRET from freezing rain cat: ', IRET

        do J=1,JMAX
        do I=1,IMAX
        IRAIN(I,J)=int(RTYPE(I,J))
        enddo
        enddo

      endif !lfull
    
! visibility 
! Moved to hourly reads for hourly writes for RTMA (03-19-2013) from 00-12 hours
! visibility from NAM parent only available every 3 hours (09-24-2013)
       print *, 'visibility read', lnest, LHR3
! Added visibility, ceiling and MSLET to output at 00h (lanl)
      if (lnest .or. LHR3 .or. REGION.EQ.'AKRT' .or. lanl) then
!       JPDS=-1;J=0
!       JPDS(5) = 020
!       JPDS(6) = 001
!       CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,VIS,IRET,ISTAT)
       JPDT(1) = 19
       JPDT(2) = 000
       JPDT(10) = 1
        J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,VIS,GFLD,ISSREF,IRET,ISTAT)

        write(0,*) 'IRET for vis: ', IRET

      if (trim(CORE).ne.'dgx') then
       print*, 'cloud ceiling height', lnest, LHR3
!       JPDS=-1;J=0
!       JPDS(5) = 007
!       JPDS(6) = 215
!       CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,CEIL,IRET,ISTAT)
       JDISC=0
       JPDT=-9999
       JPDT(1) = 3
       JPDT(2) = 5
       JPDT(10) = 215
        J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,CEIL,GFLD,ISSREF,IRET,ISTAT)
        write(0,*) 'IRET  for CEIL: ', IRET, minval(CEIL),MAXVAL(CEIL)

        print*,'min/max CEIL ', minval(CEIL),MAXVAL(CEIL)

! Membrane SLP MSLET
       print*, 'SLP', lnest, LHR3
!       JPDS=-1;J=0
!       JPDS(5) = 130
!       JPDS(6) = 102
!       CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,SLP,IRET,ISTAT)
       JDISC=0
       JPDT=-9999
       JPDT(1) = 3
       JPDT(2) = 192
       JPDT(10) = 101
        J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,SLP,GFLD,ISSREF,IRET,ISTAT)
        write(0,*) 'IRET  for MSLP: ', IRET, minval(SLP),MAXVAL(SLP)

        print*,'min/max SLP ', minval(SLP),MAXVAL(SLP)

! Skin Temperature/Sfc Temperature
       print*, 'SST', lnest, LHR3
!       JPDS=-1;J=0
!       JPDS(5) = 11
!       JPDS(6) = 1
!       CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,SST,IRET,ISTAT)
       JDISC=0
       JPDT=-9999
       JPDT(1) = 0
       JPDT(2) = 0
       JPDT(10) = 1
        J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,SST,GFLD,ISSREF,IRET,ISTAT)
        write(0,*) 'IRET  for SST: ', IRET, minval(SST),MAXVAL(SST)

        print*,'min/max SST ', minval(SST),MAXVAL(SST)

      endif ! dgx 
!     endif

!  sfc wind gust 
!     J=0
!     JPDS=-1
!     JPDS(3) = IGDNUM
!     JPDS(5) = 180 
!     JPDS(6) = 001
!     CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,GUST,IRET,ISTAT)

       JPDT(1) = 002
       JPDT(2) = 022
       JPDT(10) = -9999
       JPDT(12) = -9999

      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,GUST,GFLD,ISSREF,IRET,ISTAT)

        write(0,*) 'min/max of GUST: ', minval(GUST),maxval(GUST)
      endif

!     nests already have computed cld fracs...
      if (lnest .and. trim(CORE).ne.'GFS' .and. .not.lanl) then
!       J=0;JPDS=-1
!       JPDS(3)=IGDNUM
!       JPDS(5) = 71
!       JPDS(6) = 200
!       CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,TCLD,IRET,ISTAT)
!       JPDS(5) = 73
!       JPDS(6) = 214
!       CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,LCLD,IRET,ISTAT)
!       JPDS(5) = 74
!       JPDS(6) = 224
!       CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,MCLD,IRET,ISTAT)
!       JPDS(5) = 75
!       JPDS(6) = 234
!       CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,HCLD,IRET,ISTAT)
       JPDT(1) = 6
       JPDT(2) = 01
       JPDT(10) = -9999
       JPDT(12) = -9999
        write(0,*) 'total cloud cover'

      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,TCLD,GFLD,ISSREF,IRET,ISTAT)

        write(0,*) 'IRET from SETVAR_g2 for TCLD ', IRET

        write(0,*) 'maxval(TCLD): ', maxval(TCLD)

       JPDT(1) = 6
       JPDT(2) = 03
       JPDT(10) = -9999
       JPDT(12) = -9999
        write(0,*) 'low cloud cover'

      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,LCLD,GFLD,ISSREF,IRET,ISTAT)

        write(0,*) 'IRET from SETVAR_g2 for LCLD ', IRET

        write(0,*) 'maxval(LCLD): ', maxval(LCLD)

       JPDT(1) = 6
       JPDT(2) = 04
       JPDT(10) = -9999
       JPDT(12) = -9999
        write(0,*) 'middle cloud cover'

      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,MCLD,GFLD,ISSREF,IRET,ISTAT)

        write(0,*) 'IRET from SETVAR_g2 for MCLD ', IRET

        write(0,*) 'maxval(MCLD): ', maxval(MCLD)

       JPDT(1) = 6
       JPDT(2) = 05
       JPDT(10) = -9999
       JPDT(12) = -9999
        write(0,*) 'high cloud cover'

      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,HCLD,GFLD,ISSREF,IRET,ISTAT)

        write(0,*) 'IRET from SETVAR_g2 for HCLD ', IRET

        write(0,*) 'maxval(HCLD): ', maxval(HCLD)

       endif

! 2-m temp
!     JPDS=-1;J=0
!     JPDS(5) = 11 
!     JPDS(6) = 105 
!     JPDS(7) = 2   
!     CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,T2,IRET,ISTAT)

       JPDT(1) = 0
       JPDT(2) = 000
       JPDT(10) = 103
       JPDT(12) = 2
        J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,T2,GFLD,ISSREF,IRET,ISTAT)

        write(0,*) 'IRET for t2m: ', IRET

! 2-m spec hum
!     JPDS=-1;J=0
!     JPDS(5) = 51 
!     JPDS(6) = 105
!     JPDS(7) = 2   
!     CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,Q2,IRET,ISTAT)
       JPDT(1) = 1
       JPDT(2) = 000
       JPDT(10) = 103
       JPDT(12) = 2
        J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,Q2,GFLD,ISSREF,IRET,ISTAT)

        write(0,*) 'IRET for q2m: ', IRET

! 2-m dew point 
!     JPDS=-1;J=0
!     JPDS(5) = 17 
!     JPDS(6) = 105
!     JPDS(7) = 2   
!     CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,D2,IRET,ISTAT)

       JPDT(1) = 0
       JPDT(2) = 006
       JPDT(10) = 103
       JPDT(12) = 2
        J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,D2,GFLD,ISSREF,IRET,ISTAT)

        write(0,*) 'IRET for td2m: ', IRET

! 10-m U
!     JPDS=-1;J=0;JPDS(3)=IGDNUM
!     JPDS(5) = 33
!     JPDS(6) = 105
!     JPDS(7) = 10
!     CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,U10,IRET,ISTAT)
       JPDT(1) = 2
       JPDT(2) = 002
       JPDT(10) = 103
       JPDT(12) = 10

        J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,U10,GFLD,ISSREF,IRET,ISTAT)

! 10-m V
!     JPDS=-1;J=0;JPDS(3)=IGDNUM
!     JPDS(5) = 34
!     JPDS(6) = 105
!     JPDS(7) = 10
!     CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,V10,IRET,ISTAT)

       JPDT(1) = 2
       JPDT(2) = 003
       JPDT(10) = 103
       JPDT(12) = 10
        J=0

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,V10,GFLD,ISSREF,IRET,ISTAT)

        write(0,*) 'U10(1,1),V10(1,1): ', U10(1,1),V10(1,1)

! vegetation TYPE or Land Mask(0-1)
! Veg type Not available in some HIRESW domains ??
! Read land fraction instead (id 81)
! id 225 = Veg Type (0-16)
! to use in NDFDgrid to perform land adjustment

!     JPDS=-1;J=0;JPDS(3) = IGDNUM
!     JPDS(5) = 225
!     JPDS(6) = 001
      if (lhiresw .or. trim(CORE).eq.'GFS') JPDS(5)=81  
! Check vegtyp for GFS
!     CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,VEG,IRET,ISTAT)
       JDISC    = 2
       JPDT(1)  = 0
       JPDT(2)  = 198
       JPDT(10) = -9999
       JPDT(12) = -9999

        J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,VEG,GFLD,ISSREF,IRET,ISTAT)

        write(0,*) 'VEG(1,1): ', VEG(1,1)

      if (lfull.or.lanl) then
! lowest wet bulb zero level
!     JPDS=-1;J=0;JPDS(3) = IGDNUM
!     JPDS(5) = 7 
!     JPDS(6) = 245 
!     CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,WETFRZ,IRET,ISTAT)

        write(0,*) 'here before SETVAR for lowest wet bulb zero'

       JDISC=0
       JPDT(1) = -9999
!      JPDT(1) = 2
       JPDT(2) = 005
       JPDT(10) = 245

        J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,WETFRZ,GFLD,ISSREF,IRET,ISTAT)

        write(0,*) 'IRET for lowest wet bulb zero level: ', IRET

! Best Liftex Index 
!     JPDS=-1;J=0;JPDS(3) = IGDNUM
!     JPDS(5) = 132 
! Find out for Best Liftex Index for GFS for GRIB2
      if(trim(CORE) .eq. 'GFS') JPDS(5) = 24
!     JPDS(6) = 116 
!     CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,BLI,IRET,ISTAT)

       JDISC=0
        JPDT=-9999
       JPDT(1) = 7
       JPDT(2) = 193
       JPDT(10) = -9999
        J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,BLI,GFLD,ISSREF,IRET,ISTAT)
        write(0,*) 'IRET  for BLI: ', IRET, minval(BLI),MAXVAL(BLI)

      endif

!====================================================================
!      READ PRECIP FROM FULL (LUGB=11) or SPECIAL GRIB FILE (unit 15-21)
!  STANDARD FULL GRIB FILE PRECIP 
!      For on-Cycles it will have either a 3-hr, 6-hr, 9-hr, or 12-hr accumulation
!      For off-Cycles,  3 and 12 hr accumulations are in full grib file
!====================================================================
      if (lanl .and. trim(CORE) .eq. 'dgx')then
       print *, 'FHR ',IFHR,'  READ 3 hr PRECIP from file unit',LUGP3,LUGS3

! Initialize to zero
       THOLD=0.
       DHOLD=0.

      JPDS=-1;J=0
      NUMVP=NUMVAL;NUMVS=NUMVAL

! Read 3-hr Precip 
       JDISC=0
       JPDTN=8
       JPDT(1) = 1
       JPDT(2) = 8
       JPDT(10) = -9999
       JPDT(12) = -9999
        J=0
      CALL SETVAR_g2(LUGP3,LUGP3I,NUMVP,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,P03M,GFLD8_S,ISSREF,IRET,ISTAT)

! 3-hr Snow 

       JDISC=0
       JPDTN=8
       JPDT(1) = 1
       JPDT(2) = 13
       JPDT(10) = -9999
        J=0
      CALL SETVAR_g2(LUGS3,LUGS3I,NUMVS,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,SN03,GFLD,ISSREF,IRET,ISTAT)

      ENDIF  ! (lanl .and. trim(CORE) .eq. 'dgx')

      if (lfull) then
      JPDS=-1;J=0
      NUMVP=NUMVAL;NUMVS=NUMVAL
      IF (LHR3) THEN

! Read 3-hr Precip 
!      JPDS=-1;J=0
!      JPDS(3) = IGDNUM3 
!      NUMVP = NUMVAL3
!      JPDS(5) = 61 
!      JPDS(6) = 001 
!      print *, 'FHR ',IFHR,'  READ 3 hr PRECIP from file unit',LUGP3,LUGS3,IGDNUM3
       print *, 'FHR ',IFHR,'  READ 3 hr PRECIP from file unit',LUGP3,LUGS3
!      CALL SETVAR(LUGP3,LUGP3I,NUMVP,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,P03M,IRET,ISTAT)

       JDISC=0
       JPDTN=8
       JPDT(1) = 1
       JPDT(2) = 8
       JPDT(10) = -9999
       JPDT(12) = -9999
        J=0
      CALL SETVAR_g2(LUGP3,LUGP3I,NUMVP,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,P03M,GFLD8_S,ISSREF,IRET,ISTAT)

! 3-hr Snow 
!      JPDS=-1;J=0
!      JPDS(3) = IGDNUMSN3
!      NUMVS=NUMVALSN3
!      JPDS(5) = 65
!      JPDS(6) = 001 
!      CALL SETVAR(LUGS3,LUGS3I,NUMVS,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,SN03,IRET,ISTAT)

       JDISC=0
       JPDTN=8
       JPDT(1) = 1
       JPDT(2) = 13
       JPDT(10) = -9999
        J=0
      CALL SETVAR_g2(LUGS3,LUGS3I,NUMVS,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,SN03,GFLD,ISSREF,IRET,ISTAT)

      ENDIF

!   ON-CYC : 6 hr buckets at 12 Fhrs in special file
!   OFF-CYC: All 6 hr buckets are in special file
      IF (LHR6.OR.LHR12) THEN

! Read 6-hr Precip 
!       JPDS=-1;J=0
!       JPDS(5) = 61
!       JPDS(6) = 001 
!       JPDS(3) = IGDNUM6;NUMVP=NUMVAL6
        print *, LCYCON,IFHR,'READ 6 hr PRECIP from  file',LUGP6,IGDNUM6
!       CALL SETVAR(LUGP6,LUGP6I,NUMVP,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,P06M,IRET,ISTAT)

       JPDTN=8
       JPDT(1) = 1
       JPDT(2) = 8
       JPDT(10) = -9999
        J=0
      CALL SETVAR_g2(LUGP6,LUGP6I,NUMVP,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,P06M,GFLD,ISSREF,IRET,ISTAT)

!     6-hr snow 
!      JPDS=-1;J=0
!      JPDS(3) = IGDNUMSN6
!      NUMVS=NUMVAL6
!      JPDS(5) = 65
!      JPDS(6) = 001 
!      CALL SETVAR(LUGS6,LUGS6I,NUMVS,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,SN06,IRET,ISTAT)

       JPDTN=8
       JPDT(1) = 1
       JPDT(2) = 13
       JPDT(10) = -9999
        J=0
      CALL SETVAR_g2(LUGS6,LUGS6I,NUMVS,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,SN06,GFLD,ISSREF,IRET,ISTAT)

! Read  12-hr Precip
        IF(LHR12) THEN
!         JPDS=-1; J=0
!         JPDS(5) = 61
!         JPDS(6) = 001
!         JPDS(3) = IGDNUM
!         JPDS(3) = IGDNUM12;NUMVP=NUMVAL12
          print *, LCYCON,IFHR,'READ 12 hr PRECIP from file ',LUGP12,IGDNUM12
!         CALL SETVAR(LUGP12,LUGP12i,NUMVP,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,P12M,IRET,ISTAT)

       JPDTN=8
       JPDT(1) = 1
       JPDT(2) = 8
       JPDT(10) = -9999
        J=0
      CALL SETVAR_g2(LUGP12,LUGP12I,NUMVP,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,P12M,GFLD,ISSREF,IRET,ISTAT)
        ENDIF
      ENDIF

!  READ min/max temperature values for previous 2 hours
      print *, 'Reading temperature for previous 2 hours',LUGT1,LUGT2,IGDNUMT
!     JPDS=-1;J=0;JPDS(3) = IGDNUMT
!     JPDS(5) = 11
!     JPDS(6) = 001
!     if (inhrfrq .gt.1 ) JPDS(6)=105 ! Read 3 hrly file instead of hrly temperature file
!     CALL SETVAR(LUGT1,LUGT1I,NUMVALT,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,THOLD(:,:,2),IRET,ISTAT)

       JPDTN=0
       JPDT(1) = 0
       JPDT(2) = 000
       JPDT(10) = 1
       JPDT(12) = 0
!     if (inhrfrq .gt.1 ) JPDS(6)=105 ! Read 3 hrly file instead of hrly temperature file
      if (inhrfrq .gt.1 ) then ! Read 3 hrly file instead of hrly temperature file
       JPDT(10) = 103
       JPDT(12) = 2
      endif
        J=0
      CALL SETVAR_g2(LUGT1,LUGT1I,NUMVALT,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,THOLD(:,:,2),GFLD,ISSREF,IRET,ISTAT)

!        print*, 'THOLD(251,100,2): ', THOLD(251,100,2)
!        print*, 'THOLD(253,131,2): ', THOLD(253,131,2)

!     JPDS=-1;J=0;JPDS(3) = IGDNUMT
!     JPDS(5) = 17
!     JPDS(6) = 001
!     if (inhrfrq .gt.1 ) JPDS(6)=105 ! Read 3 hrly file instead of hrly temperature file
!     CALL SETVAR(LUGT1,LUGT1I,NUMVALT,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,DHOLD(:,:,2),IRET,ISTAT)

       JPDT(1) = 0
       JPDT(2) = 006
       JPDT(10) = 1
       JPDT(12) = 0
!     if (inhrfrq .gt.1 ) JPDS(6)=105 ! Read 3 hrly file instead of hrly temperature file
      if (inhrfrq .gt.1 ) then ! Read 3 hrly file instead of hrly temperature file
       write(0,*)'dhold inhrfrq=',inhrfrq
       JPDT(10) = 103
       JPDT(12) = 2
      endif
        J=0
      CALL SETVAR_g2(LUGT1,LUGT1I,NUMVALT,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,DHOLD(:,:,2),GFLD,ISSREF,IRET,ISTAT)

!     JPDS=-1;J=0;JPDS(3) = IGDNUMT
!     JPDS(5) = 11
!     JPDS(6) = 001
!     if (inhrfrq .gt.1 ) JPDS(6)=105 ! Read 3 hrly file instead of hrly temperature file
!     CALL SETVAR(LUGT2,LUGT2I,NUMVALT,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,THOLD(:,:,3),IRET,ISTAT)

       JPDT(1) = 0
       JPDT(2) = 000
       JPDT(10) = 1
       JPDT(12) = 0
      if (inhrfrq .gt.1 ) then ! Read 3 hrly file instead of hrly temperature file
       JPDT(10) = 103
       JPDT(12) = 2
      endif
        J=0
      CALL SETVAR_g2(LUGT2,LUGT2I,NUMVALT,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,THOLD(:,:,3),GFLD,ISSREF,IRET,ISTAT)

!     JPDS=-1;J=0;JPDS(3) = IGDNUMT
!     JPDS(5) = 17
!     JPDS(6) = 001
!     if (inhrfrq .gt.1 ) JPDS(6)=105 ! Read 3 hrly file instead of hrly temperature file
!     CALL SETVAR(LUGT2,LUGT2I,NUMVALT,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,DHOLD(:,:,3),IRET,ISTAT)

       JPDT(1) = 0
       JPDT(2) = 006
       JPDT(10) = 1
       JPDT(12) = 0
      if (inhrfrq .gt.1 ) then ! Read 3 hrly file instead of hrly temperature file
       write(0,*)'dhold inhrfrq=',inhrfrq
       JPDT(10) = 103
       JPDT(12) = 2
      endif
        J=0
      CALL SETVAR_g2(LUGT2,LUGT2I,NUMVALT,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,DHOLD(:,:,3),GFLD,ISSREF,IRET,ISTAT)

! Get min/max temperature values for full 12-hr period for F12,24...
      IF (LHR12) THEN
       IFHR4=IFHR-3
       IFHR12=IFHR-11
       KT=4
       LUGTA=LUGT3
       LUGTB=LUGT3I
       DO IIH=IFHR4,IFHR12,-1
         IF (IIH.LE.IFHR-6) THEN
           LUGTA=LUGT4
           LUGTB=LUGT4I
         ENDIF
         IF (IIH.LE.IFHR-9) THEN
           LUGTA=LUGT5
           LUGTB=LUGT5I
         ENDIF
 
!        JPDS = -1;J=0
!        JPDS(3) = IGDNUMT
!        JPDS(5) = 11 
!        JPDS(6) = 001
!        JPDS(14) = IIH   
         print *, 'READING TEMP for hr', IIH, LUGTA,LUGTB,KT
!        CALL SETVAR(LUGTA,LUGTB,NUMVALT,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,THOLD(:,:,KT),IRET,ISTAT)

       JPDT(1) = 0
       JPDT(2) = 000
!      JPDT(10) = 103
!      JPDT(12) = 2
       JPDT(10) = 1
       JPDT(12) = 0
        J=0
      CALL SETVAR_g2(LUGTA,LUGTB,NUMVALT,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,THOLD(:,:,KT),GFLD,ISSREF,IRET,ISTAT)

!        JPDS = -1;J=0
!        JPDS(3) = IGDNUMT
!        JPDS(5) = 17 
!        JPDS(6) = 001
!        JPDS(14) = IIH   
         print *, 'READING  DPT for hr', IIH, LUGTA,LUGTB,KT 
!        CALL SETVAR(LUGTA,LUGTB,NUMVALT,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,DHOLD(:,:,KT),IRET,ISTAT)

       JPDT(1) = 0
       JPDT(2) = 006
!      JPDT(10) = 103
!      JPDT(12) = 2
       JPDT(10) = 1
       JPDT(12) = 0
        J=0
      CALL SETVAR_g2(LUGTA,LUGTB,NUMVALT,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,DHOLD(:,:,KT),GFLD,ISSREF,IRET,ISTAT)
         KT=KT+1
       ENDDO
      ENDIF
      endif !lfull

!   get the vertical profile of pressure 
      print *,'READ UPPER LEVEL fields from unit ', LUGB,'KMAX',KMAX
!     J=0
        JDISC=0
!     KLTYP=109   !Hybrid vertical levels
        JPDTN=0
        JPDT=-9999
        JGDTN=-1
        JGDT=-9999

        DO LL=1,KMAX

       JPDT(1) = 003
       JPDT(2) = 000
       JPDT(10) = 105
       JPDT(12) = LL

!         JPDS=-1; JPDS(3)=IGDNUM; JPDS(5)=001; JPDS(6)=KLTYP
!         CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,PMID(:,:,LL),IRET,ISTAT)
!         J=K
        J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,PMID(:,:,LL),GFLD,ISSREF,IRET,ISTAT)

        if (IRET .eq. 0) then
        write(0,*) 'PMID(1,1,LL): ', LL, PMID(1,1,LL)
        endif

        write(0,*) 'min/max of PMID: ',LL, minval(PMID(:,:,LL)), maxval(PMID(:,:,LL))

        if (IRET .ne. 0) then
        write(0,*) 'IRET from pressure column : ', IRET
              STOP
        endif

        ENDDO

!   get the vertical profile of height 
      J=0
      DO LL=1,KMAX  
!      JPDS=-1; JPDS(3)=IGDNUM; JPDS(5)=007; JPDS(6)=KLTYP
!      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,HGHT(:,:,LL),IRET,ISTAT)
!      J=K
       JPDT(1) = 003
       JPDT(2) = 005
       JPDT(10) = 105
       JPDT(12) = LL

        J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,HGHT(:,:,LL),GFLD,ISSREF,IRET,ISTAT)
        if (IRET .eq. 0) then
        write(0,*) 'HGHT(1,1,LL): ', HGHT(1,1,LL)
        write(0,*) 'min/max of HGHT: ',LL, minval(HGHT(:,:,LL)), maxval(HGHT(:,:,LL))
        endif

      ENDDO

!   get the vertical profile of temperature
      J=0
      DO LL=1,KMAX  
!      JPDS=-1; JPDS(3)=IGDNUM; JPDS(5)=011; JPDS(6)=KLTYP
!      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,T(:,:,LL),IRET,ISTAT)
!      J=K

       JPDT(1) = 000
       JPDT(2) = 000
       JPDT(10) = 105
       JPDT(12) = LL

        J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,T(:,:,LL),GFLD,ISSREF,IRET,ISTAT)
        write(0,*) 'T(1,1,LL): ', T(1,1,LL)
        write(0,*) 'min/max of T: ',LL, minval(T(:,:,LL)), maxval(T(:,:,LL))

      ENDDO

! note points that are within bitmap
       VALIDPT=.TRUE.
         WHERE(T(:,:,1).LE.10.) VALIDPT = .FALSE.

! JTM 01-28-13: Added check for where previous temps are not at validpts
!       do i=1,imax
!       do j=1,jmax
!         if(.not.validpt(i,j)) then 
!            print *,' NOT Valid pt at :', i,j,' Temperature=',T(i,j,1)
!         endif
!       enddo
!       enddo
       print *,'VALIDPT=',validpt(20,20),'max/min Temp at lvl 1',maxval(T),minval(T)

!   get the vertical profile of q
      J=0
      DO LL=1,KMAX   
!      JPDS=-1; JPDS(3)=IGDNUM; JPDS(5)=051; JPDS(6)=KLTYP
!      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,Q(:,:,LL),IRET,ISTAT)
!      J=K
       JPDT(1) = 001
       JPDT(2) = 000
       JPDT(10) = 105
       JPDT(12) = LL

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,Q(:,:,LL),GFLD,ISSREF,IRET,ISTAT)
        J=K
        write(0,*) 'min/max of Q: ',LL, minval(Q(:,:,LL)), maxval(Q(:,:,LL))
!        write(0,*) 'Q(50,50,LL): ', LL, Q(50,50,LL)

      ENDDO

!   get the vertical profile of u 
      J=0
      DO LL=1,KMAX  
!      JPDS=-1; JPDS(3)=IGDNUM; JPDS(5)=033; JPDS(6)=KLTYP
!      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,UWND(:,:,LL),IRET,ISTAT)
!      J=K
       JPDT(1) = 002
       JPDT(2) = 002
       JPDT(10) = 105
       JPDT(12) = LL

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K, &
                     KPDS,KGDS,MASK,GRID,UWND(:,:,LL),GFLD,ISSREF,IRET,ISTAT)

       J=K
!       if (I .eq. 293 .and. J .eq. 132) then
!       write(0,*) '293,132, LL, UWND(I,J,LL): ', LL, UWND(293,132,LL)
!       endif
        write(0,*) 'min/max of UWND: ',LL, minval(UWND(:,:,LL)), maxval(UWND(:,:,LL))
        write(0,*) 'UWND(1,1,LL): ', UWND(1,1,LL)

      ENDDO

!   get the vertical profile of v
      J=0
      DO LL=1,KMAX  
!      JPDS=-1; JPDS(3)=IGDNUM; JPDS(5)=034; JPDS(6)=KLTYP
!      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,VWND(:,:,LL),IRET,ISTAT)
!      J=K
       JPDT(1) = 002
       JPDT(2) = 003
       JPDT(10) = 105
       JPDT(12) = LL

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,VWND(:,:,LL),GFLD,ISSREF,IRET,ISTAT)
       J=K
!       if (I .eq. 293 .and. J .eq. 132) then
!       write(0,*) '293,132, LL, VWND(I,J,LL): ', LL, VWND(293,132,LL)
!       endif
        write(0,*) 'VWND(1,1,LL): ', VWND(1,1,LL)
        write(0,*) 'min/max of VWND: ',LL, minval(VWND(:,:,LL)), maxval(VWND(:,:,LL))

      ENDDO

! Move to inside the limited portion as we want to write out total cloud for
! ak_rtmages at the intermediate hours. [AMG Aug 2016]
!   get the vertical profile of cloud fraction for non-nests
      if (.not. lnest .or. trim(CORE) .eq. 'GFS') then
      J=0
      DO LL=1,KMAX  
!      JPDS=-1; JPDS(3)=IGDNUM; JPDS(5)=071; JPDS(6)=KLTYP
!      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,CFR(:,:,LL),IRET,ISTAT)
!      J=K

        write(0,*) 'vertical column of cloud fraction'
       JPDT(1) = 006
!      JPDT(2) = 032
       JPDT(2) = 001
       JPDT(10) = 105
       JPDT(12) = LL

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,CFR(:,:,LL),GFLD,ISSREF,IRET,ISTAT)
       J=K

      ENDDO
      endif

        write(0,*) 'is llimited true....will avoid T850, etc: ', llimited
      if (llimited) return

!   get the vertical profile of cloud fraction for non-nests
!     if (.not. lnest .or. trim(CORE) .eq. 'GFS') then
!     J=0
!     DO LL=1,KMAX  
!      JPDS=-1; JPDS(3)=IGDNUM; JPDS(5)=071; JPDS(6)=KLTYP
!      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,CFR(:,:,LL),IRET,ISTAT)
!      J=K

!       write(0,*) 'vertical column of cloud fraction'
!      JPDT(1) = 006
!      JPDT(2) = 032
!      JPDT(2) = 001
!      JPDT(10) = 105
!      JPDT(12) = LL

!     CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
!                    KPDS,KGDS,MASK,GRID,CFR(:,:,LL),GFLD,ISSREF,IRET,ISTAT)
!      J=K

!     ENDDO
!     endif

!   950 mb temperature
!     J=0
!     JPDS(3) = IGDNUM
!     JPDS(5) = 011
!     JPDS(6) = 100
!     JPDS(7) = 950
!     CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,T950,IRET,ISTAT)

       JPDT=-9999
       JPDT(1) = 000
       JPDT(2) = 000
       JPDT(10) = 100
       JPDT(12) = 95000

       write(0,*) '950 T get'

      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,T950,GFLD,ISSREF,IRET,ISTAT)

        write(0,*) 'min/max of T950: ', minval(T950),maxval(T950)

!   850 mb temperature
!     JPDS(7) = 850
!     CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,T850,IRET,ISTAT)

       JPDT(12) = 85000
      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,T850,GFLD,ISSREF,IRET,ISTAT)

        write(0,*) 'min/max of T850: ', minval(T850),maxval(T850)

!   700 mb temperature
!     JPDS(7) = 700
!     CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,T700,IRET,ISTAT)

       JPDT(12) = 70000
      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,T700,GFLD,ISSREF,IRET,ISTAT)

        write(0,*) 'min/max of T700: ', minval(T700),maxval(T700)

!   500 mb temperature
!     JPDS(7) = 500
!     CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,T500,IRET,ISTAT)
        JPDT(12) = 50000
      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,T500,GFLD,ISSREF,IRET,ISTAT)

        write(0,*) 'min/max of T500: ', minval(T500),maxval(T500)

!   850 mb RH
!     J=0
!     JPDS(5) = 052
!     JPDS(6) = 100
!     JPDS(7) = 850
!     CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,RH850,IRET,ISTAT)

       JPDT(1) = 001
       JPDT(2) = 001
       JPDT(10) = 100
       JPDT(12) = 85000

      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,RH850,GFLD,ISSREF,IRET,ISTAT)

        write(0,*) 'min/max of RH850: ', minval(RH850),maxval(RH850)

!   700 mb RH
!     JPDS(7) = 700
!     CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,RH700,IRET,ISTAT)

       JPDT(12) = 70000

      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,RH700,GFLD,ISSREF,IRET,ISTAT)
        write(0,*) 'min/max of RH700: ', minval(RH700),maxval(RH700)

        write(0,*) 'T850, T700, T500: ', T850(1,1), T700(1,1), T500(1,1)
        write(0,*) 'RH850, RH700: ', RH850(1,1), RH700(1,1)

!  sfc wind gust 
!     J=0
!     JPDS=-1
!     JPDS(3) = IGDNUM
!     JPDS(5) = 180 
!     JPDS(6) = 001
!     CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,GUST,IRET,ISTAT)

! composite reflectivity
      if (trim(CORE).NE. 'GFS') then
!       JPDS(5) = 212
!       JPDS(6) = 200
!       CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,REFC,IRET,ISTAT)
       JPDT(1) = 16
       JPDT(2) = 196
       JPDT(10) = -9999
       JPDT(12) = -9999

      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,REFC,GFLD,ISSREF,IRET,ISTAT)

        write(0,*) 'min/max of REFC: ', minval(REFC),maxval(REFC)
        write(0,*) 'here good'

      endif

      if (trim(CORE).eq.'dgx' .and. lanl) then
          print*,'trim(CORE),lanl,ifhr,ifhrstr=',trim(CORE),lanl,ifhr,ifhrstr
          S3REF01(:,:) = 0.0
          S3REF10(:,:) = 0.0
          S3REF50(:,:) = 0.0
          S6REF01(:,:) = 0.0
          S6REF10(:,:) = 0.0
          S6REF50(:,:) = 0.0
          S12REF01(:,:) = 0.0
          S12REF10(:,:) = 0.0
          S12REF50(:,:) = 0.0
      endif

      if (lanl) return

!     nests already have computed cld fracs...
!     if (lnest .and. trim(CORE).ne.'GFS') then
!       J=0;JPDS=-1
!       JPDS(3)=IGDNUM
!       JPDS(5) = 71
!       JPDS(6) = 200
!       CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,TCLD,IRET,ISTAT)
!       JPDS(5) = 73
!       JPDS(6) = 214
!       CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,LCLD,IRET,ISTAT)
!       JPDS(5) = 74
!       JPDS(6) = 224
!       CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,MCLD,IRET,ISTAT)
!       JPDS(5) = 75
!       JPDS(6) = 234
!       CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,HCLD,IRET,ISTAT)
!      endif  

      if (HAVESREF .eq. 1)then
        write(0,*) 'to read of SREF precip'
!  READ SREF precip
      print*; print *,'READ SREF Precip Probs', LUGB2, IFHR

! 3-hr probability of .01"
!     J=0     !J= number of records to skip in SREFPCP file
!     JPDS=-1;JGDS=-1
!     JPDS(3) = IGDNUM2
!     JPDS(5) = 191 
!     JPDS(6) = 001
!     CALL SETVAR(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,S3REF01,IRET,ISTAT)
        JIDS=-9999
        JPDTN=-1
        JPDT=-9999
        JGDTN=-1
        JGDT=-9999

       J=0
       JPDT(2) = 008
       JPDT(10) = 1
       JDISC = 0

        write(0,*) 'LUGB2, LUGI2 for SREF prob: ', LUGB2, LUGI2
      CALL SETVAR_g2(LUGB2,LUGI2,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K, &
                     KPDS,KGDS,MASK,GRID,S3REF01,GFLD,ISSREF,IRET,ISTAT)

        write(0,*) 'IRET from SETVAR_g2: ', IRET


        write(0,*) 'min,max S3REF01: ', minval(S3REF01),maxval(S3REF01)
        write(0,*) 'sum(S3REF01): ', sum(S3REF01)

! probability of .1"
!     J = 1
!     JPDS=-1;JGDS=-1
!     JPDS(3) = IGDNUM2
!     JPDS(5) = 191 
!     JPDS(6) = 001
!     CALL SETVAR(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,S3REF10,IRET,ISTAT)

       J = 1
       JPDT(2) = 008
       JPDT(10) = 1
       JDISC = 0

      CALL SETVAR_g2(LUGB2,LUGI2,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K, &
                     KPDS,KGDS,MASK,GRID,S3REF10,GFLD,ISSREF,IRET,ISTAT)
      IF(IRET .NE. 0 )RETURN

! probability of 0.5"
!     J = 3
!     JPDS=-1;JGDS=-1
!     JPDS(3) = IGDNUM2
!     JPDS(5) = 191 
!     JPDS(6) = 001
!     CALL SETVAR(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,S3REF50,IRET,ISTAT)

       J = 3
       JPDT(2) = 008
       JPDT(10) = 1
       JDISC = 0

      CALL SETVAR_g2(LUGB2,LUGI2,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K, &
                     KPDS,KGDS,MASK,GRID,S3REF50,GFLD,ISSREF,IRET,ISTAT)

        write(0,*) 'minval(S3REF50),maxval(S3REF50): ', &
                    minval(S3REF50),maxval(S3REF50)

      IF (IFHR .EQ. 3) THEN
        IF (.NOT.LCYCON) THEN
          print *, 'FHR=3 so zero 6 and 12-hr sref probabilities'
!         S6REF01(M,N) = 0.0
!         S6REF10(M,N) = 0.0
!         S6REF50(M,N) = 0.0
!         S12REF01(M,N) = 0.0
!         S12REF10(M,N) = 0.0
!         S12REF50(M,N) = 0.0
          S6REF01(:,:) = 0.0
          S6REF10(:,:) = 0.0
          S6REF50(:,:) = 0.0
          S12REF01(:,:) = 0.0
          S12REF10(:,:) = 0.0
          S12REF50(:,:) = 0.0
        ENDIF
       print *, 'bailing out of sref pcp early IFHR=',IFHR
       RETURN
      ENDIF


! 6-hr probability of 0.01"
!      J = 5     
!     JPDS=-1;JGDS=-1
!     JPDS(3) = IGDNUM2
!     JPDS(5) = 191 
!     JPDS(6) = 001
!     CALL SETVAR(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,S6REF01,IRET,ISTAT)

       J = 5
       JPDT(2) = 008
       JPDT(10) = 1
       JDISC = 0

      CALL SETVAR_g2(LUGB2,LUGI2,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K, &
                     KPDS,KGDS,MASK,GRID,S6REF01,GFLD,ISSREF,IRET,ISTAT)

! 6-hr probability of 0.1"
!     J = 6
!     JPDS=-1;JGDS=-1
!     JPDS(3) = IGDNUM2
!     JPDS(5) = 191 
!     JPDS(6) = 001
!     CALL SETVAR(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,S6REF10,IRET,ISTAT)

       J = 6
       JPDT(2) = 008
       JPDT(10) = 1
       JDISC = 0

      CALL SETVAR_g2(LUGB2,LUGI2,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K, &
                     KPDS,KGDS,MASK,GRID,S6REF10,GFLD,ISSREF,IRET,ISTAT)

! 6-hr probability of 0.5"
!     J = 8
!     JPDS=-1;JGDS=-1
!     JPDS(3) = IGDNUM2
!     JPDS(5) = 191 
!     JPDS(6) = 001
!     CALL SETVAR(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,S6REF50,IRET,ISTAT)

       J = 8
       JPDT(2) = 008
       JPDT(10) = 1
       JDISC = 0

      CALL SETVAR_g2(LUGB2,LUGI2,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K, &
                     KPDS,KGDS,MASK,GRID,S6REF50,GFLD,ISSREF,IRET,ISTAT)

! 12-hr probability of 0.01"
      J = 10  
        IF (IFHR .EQ. 6 .OR. IFHR .EQ. 9) THEN
        print *, 'FHR=6 or 9 so 12-hr sref probabilities not available'
          S12REF01 = 0.0
          S12REF10 = 0.0
          S12REF50 = 0.0
          RETURN
        ENDIF

!     CALL SETVAR(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,S12REF01,IRET,ISTAT) 

       J = 10
       JPDT(2) = 008

      CALL SETVAR_g2(LUGB2,LUGI2,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K, &
                     KPDS,KGDS,MASK,GRID,S12REF01,GFLD,ISSREF,IRET,ISTAT)

! 12-hr probability of 0.1"
!      J = 11
!     CALL SETVAR(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,S12REF10,IRET,ISTAT)
       J = 11
       JPDT(2) = 008

      CALL SETVAR_g2(LUGB2,LUGI2,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K, &
                     KPDS,KGDS,MASK,GRID,S12REF10,GFLD,ISSREF,IRET,ISTAT)

! 12-hr probability of 0.5"
!     J = 13
!     CALL SETVAR(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,S12REF50,IRET,ISTAT)

       J = 13
       JPDT(2) = 008

      CALL SETVAR_g2(LUGB2,LUGI2,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K, &
                     KPDS,KGDS,MASK,GRID,S12REF50,GFLD,ISSREF,IRET,ISTAT)
      else
      write(6,*) 'SKIPPED SREF READS'
      endif

      RETURN 
      END SUBROUTINE getgrib
