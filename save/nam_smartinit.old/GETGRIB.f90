 SUBROUTINE GETGRIB(ISNOW,IZR,IIP,IRAIN,VEG,WETFRZ,  &
    P03M,P06M,P12M,SN03,SN06,S3REF01,S3REF10,S3REF50,S6REF01,  &
    S6REF10,S6REF50,S12REF01,S12REF10,S12REF50, THOLD,DHOLD,GDIN)

    use grddef
    use aset3d
    use aset2d
    use rdgrib

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

! USAGE:    CALL SMARTINIT 
!   INPUT ARGUMENT LIST:

!   OUTPUT ARGUMENT LIST:
!     NONE

!   OUTPUT FILES:
!     NONE

      TYPE (GINFO) :: GDIN
      INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)
      INTEGER YEAR,MON,DAY,IHR,DATE,IFHR

      PARAMETER(MBUF=2000000)
      CHARACTER CBUF(MBUF)
      CHARACTER*80 FNAME
      CHARACTER*4 DUM1
      LOGICAL*1 LCYCON,LHR3,LHR6,LHR12,LFULL,LANL,LLIMITED
      INTEGER JENS(200),KENS(200),CYC

!   REAL,        ALLOCATABLE   :: GRID(:)
!   LOGICAL*1,   ALLOCATABLE   :: MASK(:)
!-----------------------------------------------------------------------------------------

!  TYPE(ISET), INTENT(INOUT) :: iprcp
   INTEGER, INTENT(INOUT) :: ISNOW(:,:),IZR(:,:),IIP(:,:),IRAIN(:,:)

!  TYPE PCPSET, INTENT(INOUT) :: prcp
   REAL,    INTENT(INOUT) :: P03M(:,:),P06M(:,:),P12M(:,:),SN03(:,:),SN06(:,:)
   REAL,    INTENT(INOUT) :: WETFRZ(:,:)
   REAL,    INTENT(INOUT) :: THOLD(:,:,:),DHOLD(:,:,:)

   REAL,    INTENT(INOUT) :: VEG(:,:)

!  TYPE POPSET, INTENT(INOUT) :: pop
   REAL,    INTENT(INOUT) :: S3REF01(:,:),S3REF10(:,:),S3REF50(:,:)
   REAL,    INTENT(INOUT) :: S6REF01(:,:),S6REF10(:,:),S6REF50(:,:)
   REAL,    INTENT(INOUT) :: S12REF01(:,:),S12REF10(:,:),S12REF50(:,:)

!-----------------------------------------------------------------------------------------

!    09-2012 JTM : Modified I/O for WCOSS fort. file name nomenclature
!                  ENVVAR not needed
!                  Introduced RDHDRS and SETVAR routines to eliminate redundancies
!    10-2012 JTM : Added dynamic allocation for arrays

      LUGB=11; LUGI=12; LUGB2=13; LUGI2=14; LUGP=15; LUGPI=16
      IHROFF=0;LHR12=.FALSE.; LHR6=.FALSE.; LHR3=.FALSE.
      LFULL=.FALSE.;LANL=.FALSE.;LLIMITED=.FALSE.

      FHR=GDIN%FHR;IFHR=FHR;CYC=GDIN%CYC
      IF (IFHR.EQ.0) THEN
        LANL=.TRUE.
      ELSE
        IF (MOD(IFHR,3).EQ.0) THEN
          LFULL=.TRUE.
        ELSE
          LLIMITED=.TRUE.
        ENDIF
      ENDIF
       
      IF (CYC.EQ.12.OR.CYC.EQ.00) LCYCON=.TRUE.
      if (.not.lanl) then
      IF(LCYCON) THEN 
        IF(MOD(IFHR,12).EQ.0) LHR12=.TRUE.
        IF(MOD(IFHR,12).EQ.6 .OR. MOD(IFHR,12).EQ.9) LHR6=.TRUE.
        IF(MOD(IFHR,12).EQ.3) LHR3=.TRUE.
      ELSE
        IHROFF=IFHR
        IF (IFHR.GE.12) IHROFF=IFHR+6
        IF (MOD(IHROFF,12).EQ.O) LHR12=.TRUE.
        IF (MOD(IFHR,6).EQ.0) LHR6=.TRUE.
        IF(MOD(IFHR,3).EQ.0) LHR3=.TRUE.
      ENDIF
      endif
      print *, 'IFHR',IFHR,'LHR3',LHR3,'LHR6',LHR6,'LHR12',LHR12

!     FOR 12-hr TIMES, WE NEED 3 AND 6-HR BUCKETS AND MAX/MIN TEMP
!     DATA FOR THE PREVIOUS 11 HOURS
      IF(LHR12) THEN
       print *,'====================================================='
       print *, 'Read 3,6 hr buckets and 11 hrs of MAX,MIN TEMP', IFHR
       print *,'====================================================='

       IF (LCYCON)  THEN 
         LUGP2=17;LUGP2I=18
         LUGS=19;LUGSI=20
         LUGS2=21;LUGS2I=22
         LUGT1=23
       ELSE
         LUGS=17;LUGSI=18     ! 6 hour snow files
         LUGP2=19;LUGP2I=20   !12 hour precip files
         LUGT1=21
       ENDIF
       LUGT2=LUGT1+1
       LUGT3=LUGT1+2
       LUGT4=LUGT1+3
       LUGT5=LUGT1+4
       LUGT1I=LUGT1+5
       LUGT2I=LUGT1+6
       LUGT3I=LUGT1+7
       LUGT4I=LUGT1+8
       LUGT5I=LUGT1+9

!     FOR 6 and 9-HR TIMES, WE NEED 3-HR BUCKETS (already have the 6-hr
!     buckets in the grib file) AND 2 HOURS OF MAX/MIN TEMP DATA

!     However Off-Hour cycle runs do not have 6 hour buckets
      ELSE IF(LHR6) THEN
       print *,'======================================================='
       print *, 'Read 6hr prcp from special file, MAX,MIN TEMP', IFHR
       print *,'======================================================='
        LUGS=17; LUGSI=18; LUGT1=19; LUGT2=20; LUGT1I=21; LUGT2I=22

!     FOR F3,15,27.... WE ALREADY HAVE 3-HR BUCKETS AND NEED 2 HOURS
!     OF MAX/MIN TEMP DATA
      ELSE IF(LHR3) THEN
       print *,'====================================================='
       print *, 'Have 3 hr prcp,Read 2 hrs of MAX,MIN TEMP ', IFHR
       print *,'====================================================='
       LUGT1=15;LUGT2=16; LUGT1I=17; LUGT2I=18
      ELSE

!      IN-BETWEEN HOURS DON'T NEED ANYTHING FANCY
       print *,'====================================================='
       print *, 'IN-Between HOURS, small change ', IFHR
       print *,'====================================================='
      ENDIF
       IF (.NOT.LCYCON)THEN
         LUGS2=LUGS
         LUGS2I=LUGSI
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
      CALL RDHDRS(LUGB,LUGI,IGDNUM,GDIN,NUMVAL)
      IMAX=GDIN%IMAX;JMAX=GDIN%JMAX;KMAX=GDIN%KMAX
      NUMLEV=GDIN%KMAX
      ITOT=IMAX*JMAX
      print *,ITOT,GDIN

      if (lfull) then
      print *, 'BEGIN READING SREF HDRS'
      CALL RDHDRS(LUGB2,LUGI2,IGDNUM2,GDIN,NUMVAL2)

! GSM  READ 3-HR PRECIP AND SNOW FILES WHICH ARE NEEDED
!      IF NOT A 3-HR ACCUMULATION TIME (F15,F27,F39...) 
!      OR AN "OFF-TIME" (F13,F14,F16....)

!JTM  IF (MOD(IFHR-3,12).NE.0) THEN
      IF (.NOT.LCYCON .AND. LHR6 .OR. LCYCON .AND. MOD(IFHR-3,12).NE.0) THEN
        print *, 'READ HDR 3-hr precip from Unit ', LUGP
        CALL RDHDRS(LUGP,LUGPI,IGDNUM3,GDIN,NUMVAL3)
        CALL RDHDRS(LUGS,LUGSI,IGDNUMSN,GDIN,NUMVALSN)
        IGDNUM5=IGDNUMSN
      ENDIF

!      READ 6-HR PRECIP/SNOW FILES AT F12,F24,F36.....
!      OR 12-hr PRECIP FOR OFF-CYCLE RUNS
!JTM  IF (MOD(IFHR,12).EQ.0) THEN
      IF (LHR12) THEN
        print *, 'READING 6 or 12 hr precip from Unit ', LUGP2
        CALL RDHDRS(LUGP2,LUGP2I,IGDNUM4,GDIN,NUMVAL4)

!       OPEN 6-HR SNOW FILE
!       Not needed for off cycle files ???
        print *, 'READING 6 or 12 hr SNOW from Unit ', LUGS2
        CALL RDHDRS(LUGS2,LUGS2I,IGDNUM4,GDIN,NUMVAL4)
      ENDIF

!==================================================================
! GSM  READ TEMPERATURE HDR FILES FOR 12-HR MIN/MAX
!      READ INDEX FILE TO GET GRID SPECS
!==================================================================
!     GET GRID NUMBER FROM PDS AND PROCESS GRIB FILE
!     NOTE: WE'LL ASSUME THE GRID NUMBER IS THE SAME FOR
!     ALL OF THESE MIN/MAX FILES AND NOT DO THIS FOR EACH

      CALL RDHDRS(LUGT1,LUGT1I,IGDNUMT,GDIN,NUMVALT)
      print *, "Reading min/max Temp  UNIT:",LUGT1, LUGT1I, NUMVALT

!     Fill the max/min T/Td holders with 0's to
!      1) account for this array at times other than f12,24,36...
!      2) temporarily fill the 12th time slot, since we have only 11 here
      THOLD=0.
      DHOLD=0.

      CALL RDHDRS(LUGT2,LUGT2I,IGDNUMT,GDIN,NUMVALT)
      print *, "Reading min/max Temp  UNIT:",LUGT2, LUGT2I, NUMVALT
      IF (LHR12) THEN
        LUGT=LUGT3    
        LUGTI=LUGT3I   
        DO  IT=3,5
          CALL RDHDRS(LUGT,LUGTI,IGDNUMT,GDIN,NUMVALT)
          print *, "Reading min/max Temp  UNIT:",LUGT, LUGTI, NUMVALT
          LUGT=LUGT+1
          LUGTI=LUGTI+1
        ENDDO 
      ENDIF
      endif !LFULL

!  get sfc height 
      ALLOCATE (GRID(ITOT),MASK(ITOT),STAT=kret)
      print *,'kret',kret
      J=0;JPDS=-1;JGDS=-1;JPDS(3) = IGDNUM
      JPDS(5) = 007
      JPDS(6) = 001
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,ZSFC,IRET,ISTAT)
        WHERE (ZSFC < 0.0) ZSFC=0.0

! get surface pressure
      JPDS(5) = 001
      JPDS(6) = 001
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,PSFC,IRET,ISTAT)

! get 4 INTEGER precip types 
      if (lfull) then
      J=0;JPDS=-1;JPDS(3)=IGDNUM 
      JPDS(5) = 143 
      JPDS(6) = 001

!     Get INTEGER GRIB Variable  
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        DO KK = 1, ITOT
          IF(MOD(KK,IMAX).EQ.0) THEN
            M=IMAX
            N=INT(KK/IMAX)
          ELSE
            M=MOD(KK,IMAX)
            N=INT(KK/IMAX) + 1
          ENDIF
          ISNOW(M,N) = GRID(KK)
        ENDDO
        WRITE(6,*) JPDS(5),JPDS(6),JPDS(7),J,KF,K
      ELSE
       WRITE(6,*)'COULD NOT UNPACK VARB',J,JPDS(3),JPDS(5),IRET
       ISTAT = IRET
      ENDIF

      JPDS=-1;J=0
      JPDS(5) = 142
      JPDS(6) = 001
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        DO KK = 1, ITOT
          IF(MOD(KK,IMAX).EQ.0) THEN
            M=IMAX
            N=INT(KK/IMAX)
          ELSE
            M=MOD(KK,IMAX)
            N=INT(KK/IMAX) + 1
          ENDIF
          IIP(M,N) = GRID(KK)
        ENDDO
        WRITE(6,*) JPDS(5),JPDS(6),JPDS(7),J,KF,K
      ELSE
       WRITE(6,*)'COULD NOT UNPACK VARB',J,JPDS(3),JPDS(5),IRET
       ISTAT = IRET
      ENDIF

! frz rain
      JPDS=-1;J=0
      JPDS(5) = 141
      JPDS(6) = 001
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K, KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        DO KK = 1, ITOT
          IF(MOD(KK,IMAX).EQ.0) THEN
            M=IMAX
            N=INT(KK/IMAX)
          ELSE
            M=MOD(KK,IMAX)
            N=INT(KK/IMAX) + 1
          ENDIF
          IZR(M,N) = GRID(KK)
        ENDDO
        WRITE(6,*) JPDS(5),JPDS(6),JPDS(7),J,KF,K
      ELSE
       WRITE(6,*)'COULD NOT UNPACK VARB',J,JPDS(3),JPDS(5),IRET
       ISTAT = IRET
      ENDIF

! rain
      JPDS=-1;J=0
      JPDS(5) = 140
      JPDS(6) = 001
!     Get INTEGER GRIB Variable  
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        DO KK = 1, ITOT
          IF(MOD(KK,IMAX).EQ.0) THEN
            M=IMAX
            N=INT(KK/IMAX)
          ELSE
            M=MOD(KK,IMAX)
            N=INT(KK/IMAX) + 1
          ENDIF
          IRAIN(M,N) = GRID(KK)
        ENDDO
        WRITE(6,*) JPDS(5),JPDS(6),JPDS(7),J,KF,K
      ELSE
       WRITE(6,*)'COULD NOT UNPACK VARB',J,JPDS(3),JPDS(5),IRET
       ISTAT = IRET
      ENDIF

      endif !lfull
    
      if (lfull.or.lanl) then
! lowest wet bulb zero level
      JPDS=-1;J=0
      JPDS(5) = 7 
      JPDS(6) = 245 
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,WETFRZ,IRET,ISTAT)

! visibility 
      JPDS=-1;J=0
      JPDS(5) = 020
      JPDS(6) = 001
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,VIS,IRET,ISTAT)
      endif

! 2-m temp
      JPDS=-1;J=0
      JPDS(5) = 11 
      JPDS(6) = 105 
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,T2,IRET,ISTAT)

! 2-m spec hum
      JPDS=-1;J=0
      JPDS(5) = 51 
      JPDS(6) = 105
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,Q2,IRET,ISTAT)

! 2-m dew point 
      JPDS=-1;J=0
      JPDS(5) = 17 
      JPDS(6) = 105
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,D2,IRET,ISTAT)

! 10-m U
      JPDS=-1;J=0;JPDS(3)=IGDNUM
      JPDS(5) = 33
      JPDS(6) = 105
      JPDS(7) = 10
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,U10,IRET,ISTAT)

! 10-m V
      JPDS=-1;J=0;JPDS(3)=IGDNUM
      JPDS(5) = 34
      JPDS(6) = 105
      JPDS(7) = 10
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,V10,IRET,ISTAT)

! vegetation fraction
      JPDS=-1;J=0;JPDS(3) = IGDNUM
      JPDS(5) = 225
      JPDS(6) = 001

      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,VEG,IRET,ISTAT)

! Best Liftex Index 
      if (lfull.or.lanl) then
      JPDS=-1;J=0;JPDS(3) = IGDNUM
      JPDS(5) = 132 
      JPDS(6) = 116 
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,BLI,IRET,ISTAT)
      endif

!====================================================================
!      READ PRECIP FROM SPECIAL GRIB FILE (unit LUGP)
!====================================================================
      if (lfull) then
      JPDS=-1;J=0;JPDS(3) = IGDNUM
      IF (LCYCON .AND. .NOT.LHR3) THEN

! 3-hr Precip from special file 
       JPDS=-1;J=0
       JPDS(3) = IGDNUM3
       JPDS(5) = 61 
       JPDS(6) = 001 
       print *, IFHR,'READ 3 hr PRECIP from Special file unit',LUGP
       CALL SETVAR(LUGP,LUGPI,NUMVAL3,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,P03M,IRET,ISTAT)

! 3-hr Snow from special file
       JPDS=-1;J=0
       JPDS(3) = IGDNUMSN
       JPDS(5) = 65
       JPDS(6) = 001 
       CALL SETVAR(LUGS,LUGSI,NUMVALSN,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,SN03,IRET,ISTAT)
      ENDIF

! 6-hr Precip from special file
!     ON-CYC : 6 hr buckets at 12 Fhrs in special file
!     OFF-CYC: All 6 hr buckets are in special file
      IF (LHR12 .OR. LHR6.AND..NOT.LCYCON) THEN
        JPDS=-1;J=0
        JPDS(5) = 61
        JPDS(6) = 001 
        JPDS(3) = IGDNUM4;LUGPP=LUGP2;LUGPPI=LUGP2I;NUMVP=NUMVAL4
        IF (.NOT.LCYCON) JPDS(3)=IGDNUM3;LUGPP=LUGP;LUGPPI=LUGPI;NUMVP=NUMVAL3

        print *, LCYCON,IFHR,'READ 6 hr PRECIP from Spec file',LUGPP
        CALL SETVAR(LUGPP,LUGPPI,NUMVP,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,P06M,IRET,ISTAT)

!     6-hr snow from special file
       JPDS=-1;J=0
       JPDS(3) = IGDNUMSN
       JPDS(5) = 65
       JPDS(6) = 001 
       NUMVS=NUMVAL4
       IF (.NOT.LCYCON) NUMVS=NUMVALSN
       CALL SETVAR(LUGS2,LUGS2I,NUMVS,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,SN06,IRET,ISTAT)
      ELSE
       IF (.NOT.LCYCON) THEN
        P06M=0.0
        SN06=-99
       ENDIF
      ENDIF

!=================================================================
!      STANDARD FULL GRIB FILE PRECIP READS 
!      For on-Cycles it will have either a 3-hr, 6-hr, 9-hr, or 12-hr accumulation
!      For off-Cycles, only 3-hr accumulations are in full grib file
!=================================================================
      JPDS=-1; J=0;JPDS(3) = IGDNUM

! 12-hr Precip
      IF (LHR12) THEN
        JPDS(5) = 61
        JPDS(6) = 001
        IF(LCYCON) THEN
          JPDS(3) = IGDNUM
          print *, LCYCON,IFHR,'READ 12 hr PRECIP from full file ',LUGB
          CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,P12M,IRET,ISTAT)
        ELSE
          print *, LCYCON,IFHR,'READ 12 hr PRECIP from Spec file ',LUGP2
          JPDS(3) = IGDNUM4
          CALL SETVAR(LUGP2,LUGP2I,NUMVAL4,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,P12M,IRET,ISTAT)
        ENDIF

! 6-hr Precip and Snow for on-Cycle run in std grib file
      ELSE IF (LCYCON.AND.LHR6) THEN
        J=0;JPDS=-1;JPDS(3) = IGDNUM
        JPDS(5) = 61
        JPDS(6) = 001
        print *, LCYCON,IFHR,'READ 06 hr PRECIP from full file ',LUGB
        CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,P06M,IRET,ISTAT)
        P12M=0.0

        J=0;JPDS=-1;JPDS(3) = IGDNUM
        JPDS(5) = 65
        JPDS(6) = 001
        CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,SN06,IRET,ISTAT)

      ELSE IF (LCYCON.AND.MOD(IFHR,12).EQ.9) THEN

!  don't need 9-hr accumulation and don't have 6 or 12
         print *, 'ON-CYCLE: No 6 or 12 hr buckets',IFHR
         P06M=0.0
         P12M=0.0
      ENDIF

! 3-hr Precip from std file
      IF (LCYCON.AND.LHR3 .OR. .NOT.LCYCON) THEN
        J=0;JPDS=-1;JPDS(3) = IGDNUM
        JPDS(5) = 61
        JPDS(6) = 001
        print *, 'IFHR',IFHR,'READ 03 hr QPF from full file ',LUGB
        CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,P03M,IRET,ISTAT)
        IF (LCYCON) THEN
         P06M=0.0
         P12M=0.0
        ENDIF

        J=0;JPDS=-1;JPDS(3) = IGDNUM
        JPDS(5) = 65
        JPDS(6) = 001
        CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,SN03,IRET,ISTAT)
        IF (LCYCON) SN06=0.0
      ENDIF

!  READ min/max temperature values for previous 2 hours
      print *, 'Reading max/min for previous 2 hours',LUGT1,LUGT2
      JPDS=-1;J=0;JPDS(3) = IGDNUM
      JPDS(5) = 11
      JPDS(6) = 001
      CALL SETVAR(LUGT1,LUGT1I,NUMVALT,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,THOLD(:,:,2),IRET,ISTAT)

      JPDS=-1;J=0;JPDS(3) = IGDNUM
      JPDS(5) = 17
      JPDS(6) = 001
      CALL SETVAR(LUGT1,LUGT1I,NUMVALT,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,DHOLD(:,:,2),IRET,ISTAT)

      JPDS=-1;J=0;JPDS(3) = IGDNUM
      JPDS(5) = 11
      JPDS(6) = 001
      CALL SETVAR(LUGT2,LUGT2I,NUMVALT,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,THOLD(:,:,3),IRET,ISTAT)

      JPDS=-1;J=0;JPDS(3) = IGDNUM
      JPDS(5) = 17
      JPDS(6) = 001
      CALL SETVAR(LUGT2,LUGT2I,NUMVALT,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,DHOLD(:,:,3),IRET,ISTAT)

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
 
         JPDS = -1;J=0
         JPDS(3) = IGDNUM
         JPDS(5) = 11 
         JPDS(6) = 001
         JPDS(14) = IIH   
         print *, 'READING TEMP for hr', IIH, LUGTA,LUGTB,KT
         CALL SETVAR(LUGTA,LUGTB,NUMVALT,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,THOLD(:,:,KT),IRET,ISTAT)
         JPDS = -1;J=0
         JPDS(3) = IGDNUM
         JPDS(5) = 17 
         JPDS(6) = 001
         JPDS(14) = IIH   
         CALL SETVAR(LUGTA,LUGTB,NUMVALT,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,DHOLD(:,:,KT),IRET,ISTAT)
         KT=KT+1
       ENDDO
      ENDIF
      endif !lfull

!   get the vertical profile of pressure 
      print *,'READ UPPER LEVEL fields from unit ', LUGB
      J=0
      DO LL=1,KMAX
       JPDS=-1; JPDS(3)=IGDNUM; JPDS(5)=001; JPDS(6)=109
       CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,PMID(:,:,LL),IRET,ISTAT)
       J=K
      ENDDO

!   get the vertical profile of height 
      J=0
      DO LL=1,KMAX  
       JPDS=-1; JPDS(3)=IGDNUM; JPDS(5)=007; JPDS(6)=109
       CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,HGHT(:,:,LL),IRET,ISTAT)
       J=K
      ENDDO

!   get the vertical profile of temperature
      J=0
      DO LL=1,KMAX  
       JPDS=-1; JPDS(3)=IGDNUM; JPDS(5)=011; JPDS(6)=109
       CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,T(:,:,LL),IRET,ISTAT)
       J=K
      ENDDO

!   get the vertical profile of q
      J=0
      DO LL=1,KMAX   
       JPDS=-1; JPDS(3)=IGDNUM; JPDS(5)=051; JPDS(6)=109
       CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,Q(:,:,LL),IRET,ISTAT)
       J=K
      ENDDO

!   get the vertical profile of u 
      J=0
      DO LL=1,KMAX  
       JPDS=-1; JPDS(3)=IGDNUM; JPDS(5)=033; JPDS(6)=109
       CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,UWND(:,:,LL),IRET,ISTAT)
       J=K
      ENDDO

!   get the vertical profile of v
      J=0
      DO LL=1,KMAX  
       JPDS=-1; JPDS(3)=IGDNUM; JPDS(5)=034; JPDS(6)=109
       CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,VWND(:,:,LL),IRET,ISTAT)
       J=K
      ENDDO
      if (llimited) return

!   get the vertical profile of cloud fraction 
      J=0
      DO LL=1,KMAX  
       JPDS=-1; JPDS(3)=IGDNUM; JPDS(5)=071; JPDS(6)=109
       CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,CFR(:,:,LL),IRET,ISTAT)
       J=K
      ENDDO

!   950 mb temperature
      J=0
      JPDS(3) = IGDNUM
      JPDS(5) = 011
      JPDS(6) = 100
      JPDS(7) = 950
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,T950,IRET,ISTAT)

!   850 mb temperature
      JPDS(7) = 850
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,T850,IRET,ISTAT)

!   700 mb temperature
      JPDS(7) = 700
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,T700,IRET,ISTAT)

!   500 mb temperature
      JPDS(7) = 500
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,T500,IRET,ISTAT)

!   850 mb RH
      J=0
      JPDS(5) = 052
      JPDS(6) = 100
      JPDS(7) = 850
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,RH850,IRET,ISTAT)

!   700 mb RH
      JPDS(7) = 700
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,RH700,IRET,ISTAT)

!  sfc wind gust 
      J=0
      JPDS=-1
      JPDS(3) = IGDNUM
      JPDS(5) = 180 
      JPDS(6) = 001
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,GUST,IRET,ISTAT)

! composite reflectivity
      JPDS(5) = 212
      JPDS(6) = 200
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,REFC,IRET,ISTAT)

      if (lanl) return
!  READ SREF precip
      print*; print *,'READ SREF Precip Probs', LUGB2

! 3-hr probability of .01"
      J=0     !J= number of records to skip in SREFPCP file
      JPDS=-1;JGDS=-1
      JPDS(3) = IGDNUM2
      JPDS(5) = 191 
      JPDS(6) = 001
      CALL SETVAR(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,S3REF01,IRET,ISTAT)

! probability of .1"
      J = 2
      JPDS=-1;JGDS=-1
      JPDS(3) = IGDNUM2
      JPDS(5) = 191 
      JPDS(6) = 001
      CALL SETVAR(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,S3REF10,IRET,ISTAT)
      IF(IRET .NE. 0 )RETURN

! probability of 0.5"
      J = 4
      JPDS=-1;JGDS=-1
      JPDS(3) = IGDNUM2
      JPDS(5) = 191 
      JPDS(6) = 001
      CALL SETVAR(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,S3REF50,IRET,ISTAT)

      IF (IFHR .EQ. 3) THEN
        IF (.NOT.LCYCON) THEN
          print *, 'FHR=3 so zero 6 and 12-hr sref probabilities'
          S6REF01(M,N) = 0.0
          S6REF10(M,N) = 0.0
          S6REF50(M,N) = 0.0
          S12REF01(M,N) = 0.0
          S12REF10(M,N) = 0.0
          S12REF50(M,N) = 0.0
        ENDIF
       print *, 'bailing out of sref pcp early IFHR=',IFHR
       RETURN
      ENDIF

! 6-hr probability of 0.01"
      J = 5
      JPDS=-1;JGDS=-1
      JPDS(3) = IGDNUM2
      JPDS(5) = 191 
      JPDS(6) = 001
      CALL SETVAR(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,S6REF01,IRET,ISTAT)

! 6-hr probability of 0.1"
      J = J+2
      JPDS=-1;JGDS=-1
      JPDS(3) = IGDNUM2
      JPDS(5) = 191 
      JPDS(6) = 001
      CALL SETVAR(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,S6REF10,IRET,ISTAT)

! 6-hr probability of 0.5"
      J = J+2
      JPDS=-1;JGDS=-1
      JPDS(3) = IGDNUM2
      JPDS(5) = 191 
      JPDS(6) = 001
      CALL SETVAR(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,S6REF50,IRET,ISTAT)

! 12-hr probability of 0.01"
      J = 10
        IF (IFHR .EQ. 6 .OR. IFHR .EQ. 9) THEN
        print *, 'FHR=6 or 9 so 12-hr sref probabilities not available'
          S12REF01 = 0.0
          S12REF10 = 0.0
          S12REF50 = 0.0
          RETURN
        ENDIF

      CALL SETVAR(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,S12REF01,IRET,ISTAT) 
! 12-hr probability of 0.1"
      J = J+2 
      CALL SETVAR(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,S12REF10,IRET,ISTAT)

! 12-hr probability of 0.5"
      J = J+2
      CALL SETVAR(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,S12REF50,IRET,ISTAT)

      RETURN 
      END SUBROUTINE getgrib
