      SUBROUTINE GETGRIB_OFF(PSFC,ZSFC,PMID,HGHT,T,Q,UWND,VWND,
     X    CFR,ISNOW,IZR,IIP,IRAIN,T2,Q2,D2,U10,V10,VEG,BLI,
     X    WETFRZ,VIS,T950,T850,T700,T500,RH850,RH700,GUST,REFC,
     X    P03M,P06M,P12M,SN03,SN06,S3REF01,S3REF10,S3REF50,
     X    S6REF01,S6REF10,S6REF50,S12REF01,S12REF10,S12REF50,
     X    THOLD,DHOLD,DATE,IFHR)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C SUBPROGRAM:    GETGRIB    CREATES NDFD FILES 
C   PRGRMMR: MANIKIN           ORG: W/NP22     DATE: 06-09-14
C
C ABSTRACT:
C   .
C
C PROGRAM HISTORY LOG:
C   06-09-14  G MANIKIN  - ADAPT CODE TO NAM 
C
C USAGE:    CALL SMARTINIT 
C   INPUT ARGUMENT LIST:
C
C   OUTPUT ARGUMENT LIST:
C     NONE
C
C   OUTPUT FILES:
C     NONE

      PARAMETER(ILIM=1073,JLIM=689,MAXLEV=60)
      PARAMETER(ITOT=ILIM*JLIM)
      DIMENSION GRID(ITOT),DIFF(5)
      DIMENSION INCDAT(8),JNCDAT(8)
      INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)
      INTEGER LEVS(MAXLEV),IVAR(5),YEAR,MON,DAY,IHR,DATE,IFHR
      LOGICAL*1 MASK(ITOT), MASK2(ITOT)
C
      PARAMETER(MBUF=2000000,JF=1000000)
      CHARACTER CBUF(MBUF)
      CHARACTER CBUF2(MBUF)
      CHARACTER*11 ENVVAR
      CHARACTER*80 FNAME
      CHARACTER*4 DUM1
      LOGICAL*1 LB(JF)
      REAL F(JF)
      PARAMETER(MSK1=32000,MSK2=4000)
      INTEGER JENS(200),KENS(200)
      INTEGER ISNOW(ILIM,JLIM),IRAIN(ILIM,JLIM),IIP(ILIM,JLIM),
     x       IZR(ILIM,JLIM)
      DIMENSION ZSFC(ILIM,JLIM),T(ILIM,JLIM,MAXLEV),PSFC(ILIM,JLIM),
     x  Q(ILIM,JLIM,MAXLEV),PMID(ILIM,JLIM,MAXLEV),WETFRZ(ILIM,JLIM),
     x  UWND(ILIM,JLIM,MAXLEV),VWND(ILIM,JLIM,MAXLEV),VIS(ILIM,JLIM),
     x  T2(ILIM,JLIM),Q2(ILIM,JLIM),D2(ILIM,JLIM),
     x  U10(ILIM,JLIM),V10(ILIM,JLIM),
     x  HGHT(ILIM,JLIM,MAXLEV),BLI(ILIM,JLIM),P12M(ILIM,JLIM),
     x  T950(ILIM,JLIM),T850(ILIM,JLIM),T700(ILIM,JLIM),T500(ILIM,JLIM),
     x  CFR(ILIM,JLIM,MAXLEV),RH850(ILIM,JLIM),RH700(ILIM,JLIM),
     x  P03M(ILIM,JLIM),VEG(ILIM,JLIM),P06M(ILIM,JLIM),
     x  THOLD(ILIM,JLIM,12),DHOLD(ILIM,JLIM,12),REFC(ILIM,JLIM),
     x  SN03(ILIM,JLIM),SN06(ILIM,JLIM),P09M(ILIM,JLIM),GUST(ILIM,JLIM)
      DIMENSION S3REF01(ILIM,JLIM),S3REF10(ILIM,JLIM),
     x  S3REF50(ILIM,JLIM),S6REF01(ILIM,JLIM),S6REF10(ILIM,JLIM),
     x  S6REF50(ILIM,JLIM),S12REF01(ILIM,JLIM),
     x  S12REF10(ILIM,JLIM),S12REF50(ILIM,JLIM)
C
      fname='fort.  '
      NUMLEV=MAXLEV
c  fill the max/min T/Td holders with 0's to
c    1) account for this array at times other than f18,30,42...
c    2) temporarily fill the 12th time slot, since we have only 11 here
c  note that this array will only have 5 filled slots at f06
       DO J=1,JLIM
       DO I=1,ILIM
        DO L=1,12
          THOLD(I,J,L)=0.
          DHOLD(I,J,L)=0.
        ENDDO
       ENDDO
       ENDDO 
C
      print *, 'IFHR ', IFHR
      IF (IFHR .EQ. 18 .OR. IFHR .EQ. 30 .OR. IFHR .EQ. 42 .OR.
     x   IFHR .EQ. 54 .OR. IFHR .EQ. 66 .OR. IFHR .EQ. 78) THEN
       print *, 'the full deal ', IFHR
       LUGB=11
       LUGI=12
       LUGB2=13
       LUGI2=14
       LUGP=15
       LUGPI=16
       LUGS=17
       LUGSI=18
       LUGP2=19
       LUGP2I=20
       LUGT1=21
       LUGT2=22
       LUGT3=23
       LUGT4=24
       LUGT5=25
       LUGT1I=26
       LUGT2I=27
       LUGT3I=28
       LUGT4I=29
       LUGT5I=30
      ELSE IF(MOD(IFHR,6).EQ.0) THEN
       print *, 'F6 plan ', IFHR
       LUGB=11
       LUGI=12
       LUGB2=13
       LUGI2=14
       LUGP=15
       LUGPI=16
       LUGS=17
       LUGSI=18
       LUGT1=19
       LUGT2=20
       LUGT1I=21
       LUGT2I=22
      ELSE IF(MOD(IFHR,3).EQ.0) THEN
       print *, 'F3 plan ', IFHR
       LUGB=11
       LUGI=12
       LUGB2=13
       LUGI2=14
       LUGT1=15
       LUGT2=16
       LUGT1I=17
       LUGT2I=18
      ELSE
       print *, 'small change ', IFHR
       LUGB=11
       LUGI=12
       LUGB2=13
       LUGI2=14
      ENDIF

      OPEN(49,file='DATE',form='formatted')
      READ(49,200) DUM1,DATE
      CLOSE(49)
 200  FORMAT(A4,2X,I10)
      year=int(date/1000000)
      mon=int(int(mod(date,1000000)/100)/100)
      day=int(mod(date,10000)/100)
      ihr=mod(date,100)
      print *, 'date ', DATE,YEAR,MON,DAY,IHR 

C GSM  READ NAM FILE 
C  READ INDEX FILE TO GET GRID SPECS
C
      IRGI = 1
      IRGS = 1
      KMAX = 0
      JR=0
      KSKIP = 0

      WRITE(FNAME(6:7),FMT='(I2)')LUGB
      CALL BAOPEN(LUGB,FNAME,IRETGB)
      WRITE(FNAME(6:7),FMT='(I2)')LUGI
      CALL BAOPEN(LUGI,FNAME,IRETGI)

      CALL GETGI(LUGI,KSKIP,MBUF,CBUF,NLEN,NNUM,IRGI)
      write(6,*)' IRET FROM GETGI ',IRGI
      IF(IRGI .NE. 0) THEN
        WRITE(6,*)' PROBLEMS READING GRIB INDEX FILE SO ABORT'
        ISTAT = IRGI
        RETURN
      ENDIF
c      REWIND LUGI

      DO K = 1, NNUM
        JR = K - 1
        JPDS = -1
        JGDS = -1
        CALL GETGB1S(CBUF,NLEN,NNUM,JR,JPDS,JGDS,JENS,
     &               KR,KPDS,KGDS,KENS,LSKIP,LGRIB,IRGS)
        write(6,*)' IRET FROM GETGB1S ',IRGS
        IF(IRGI .NE. 0) THEN
          WRITE(6,*)' PROBLEMS ON 1ST READ OF GRIB FILE SO ABORT'
          ISTAT = IRGS
          RETURN
        ENDIF
C
      ENDDO

C    GET GRID NUMBER FROM PDS
C
      IGDNUM = KPDS(3)
C
C   PROCESS THE GRIB FILE
C
      IMAX = KGDS(2)
      JMAX = KGDS(3)
      NUMVAL = IMAX*JMAX
      KMAX = MAXLEV
      WRITE(6,280) IMAX,JMAX,NUMLEV,KMAX
  280 FORMAT(' IMAX,JMAX,NUMLEV,KMAX ',5I4)

      print *, 'about to read sref'
C GSM  READ SREF FILE
C  READ INDEX FILE TO GET GRID SPECS
C
      IRGI = 1
      IRGS = 1
      KMAX = 0
      JR=0
      KSKIP = 0
      WRITE(FNAME(6:7),FMT='(I2)')LUGB2
      CALL BAOPEN(LUGB2,FNAME,IRETGB)
      WRITE(FNAME(6:7),FMT='(I2)')LUGI2
      CALL BAOPEN(LUGI2,FNAME,IRETGI)

      CALL GETGI(LUGI2,KSKIP,MBUF,CBUF,NLEN,NNUM,IRGI)
      write(6,*)' IRET FROM GETGI ',IRGI
      IF(IRGI .NE. 0) THEN
        WRITE(6,*)' PROBLEMS READING GRIB INDEX FILE SO ABORT'
        ISTAT = IRGI
      ENDIF
c      REWIND LUGI2

      DO K = 1, NNUM
        JR = K - 1
        JPDS = -1
        JGDS = -1
        CALL GETGB1S(CBUF,NLEN,NNUM,JR,JPDS,JGDS,JENS,
     &               KR,KPDS,KGDS,KENS,LSKIP,LGRIB,IRGS)
        write(6,*)' IRET FROM GETGB1S ',IRGS
        IF(IRGI .NE. 0) THEN
          WRITE(6,*)' PROBLEMS ON 1ST READ OF GRIB FILE SO ABORT'
          ISTAT = IRGS
          RETURN
        ENDIF
C
      ENDDO
                                                                                   
C    GET GRID NUMBER FROM PDS
C
      IGDNUM2 = KPDS(3)
C
C   PROCESS THE GRIB FILE
C
      IMAX = KGDS(2)
      JMAX = KGDS(3)
      NUMVAL2 = IMAX*JMAX
      KMAX = MAXLEV
      WRITE(6,281) IMAX,JMAX,NUMLEV,KMAX
  281 FORMAT(' IMAX,JMAX,NUMLEV,KMAX ',5I4)

C GSM  READ 6-HR PRECIP AND SNOW FILES WHICH ARE NEEDED
C      AT F6,F12,F18,
C
      IF (MOD(IFHR,6).EQ.0) THEN
       IRGI = 1
       IRGS = 1
       KMAX = 0
       JR=0
       KSKIP = 0
      WRITE(FNAME(6:7),FMT='(I2)')LUGP
      CALL BAOPEN(LUGP,FNAME,IRETGB)
      WRITE(FNAME(6:7),FMT='(I2)')LUGPI
      CALL BAOPEN(LUGPI,FNAME,IRETGI)

       CALL GETGI(LUGPI,KSKIP,MBUF,CBUF,NLEN,NNUM,IRGI)
       write(6,*)' IRET FROM GETGI ',IRGI
       IF(IRGI .NE. 0) THEN
         WRITE(6,*)' PROBLEMS READING GRIB INDEX FILE SO ABORT'
         ISTAT = IRGI
       ENDIF
c      REWIND LUGP
       DO K = 1, NNUM
         JR = K - 1
         JPDS = -1
         JGDS = -1
         CALL GETGB1S(CBUF,NLEN,NNUM,JR,JPDS,JGDS,JENS,
     &               KR,KPDS,KGDS,KENS,LSKIP,LGRIB,IRGS)
         write(6,*)' IRET FROM GETGB1S ',IRGS
         IF(IRGI .NE. 0) THEN
           WRITE(6,*)' PROBLEMS ON 1ST READ OF PCP1 FILE SO ABORT'
           ISTAT = IRGS
           RETURN
         ENDIF
       ENDDO

C    GET GRID NUMBER FROM PDS AND PROCESS GRIB FILE
C
      IGDNUM3 = KPDS(3)
      IMAX = KGDS(2)
      JMAX = KGDS(3)
      NUMVAL3 = IMAX*JMAX
      KMAX = MAXLEV

       IRGI = 1
       IRGS = 1
       KMAX = 0
       JR=0
       KSKIP = 0
       WRITE(FNAME(6:7),FMT='(I2)')LUGS
       CALL BAOPEN(LUGS,FNAME,IRETGB)
       WRITE(FNAME(6:7),FMT='(I2)')LUGSI
       CALL BAOPEN(LUGSI,FNAME,IRETGI)
       CALL GETGI(LUGSI,KSKIP,MBUF,CBUF,NLEN,NNUM,IRGI)
       write(6,*)' IRET FROM GETGI ',IRGI,LUGS
       IF(IRGI .NE. 0) THEN
         WRITE(6,*)' PROBLEMS READING GRIB INDEX FILE SO ABORT',LUGS
         ISTAT = IRGI
       ENDIF

       DO K = 1, NNUM
         JR = K - 1
         JPDS = -1
         JGDS = -1
         CALL GETGB1S(CBUF,NLEN,NNUM,JR,JPDS,JGDS,JENS,
     &               KR,KPDS,KGDS,KENS,LSKIP,LGRIB,IRGS)
         write(6,*)' IRET FROM GETGB1S ',IRGS
         IF(IRGI .NE. 0) THEN
           WRITE(6,*)' PROBLEMS ON 1ST READ OF PCP1 FILE SO ABORT'
           ISTAT = IRGS
           RETURN
         ENDIF
       ENDDO

       IGDNUMSN = KPDS(3)
       IMAX = KGDS(2)
       JMAX = KGDS(3)
       NUMVALSN = IMAX*JMAX
       KMAX = MAXLEV
      ENDIF

      IF (IFHR .EQ. 18 .OR. IFHR .EQ. 30 .OR. IFHR .EQ. 42 .OR.
     x   IFHR .EQ. 54 .OR. IFHR .EQ. 66 .OR. IFHR .EQ. 78) THEN
c      read 12-hr precip
       IRGI = 1
       IRGS = 1
       KMAX = 0
       JR=0
       KSKIP = 0
      WRITE(FNAME(6:7),FMT='(I2)')LUGP2
       CALL BAOPEN(LUGP2,FNAME,IRETGB)
      WRITE(FNAME(6:7),FMT='(I2)')LUGP2I
       CALL BAOPEN(LUGP2I,FNAME,IRETGI)
       CALL GETGI(LUGP2I,KSKIP,MBUF,CBUF,NLEN,NNUM,IRGI)
       write(6,*)' IRET FROM GETGI ',IRGI
       IF(IRGI .NE. 0) THEN
         WRITE(6,*)' PROBLEMS READING GRIB INDEX FILE SO ABORT'
         ISTAT = IRGI
       ENDIF
c      REWIND LUGP2
       DO K = 1, NNUM
         JR = K - 1
         JPDS = -1
         JGDS = -1
         CALL GETGB1S(CBUF,NLEN,NNUM,JR,JPDS,JGDS,JENS,
     &               KR,KPDS,KGDS,KENS,LSKIP,LGRIB,IRGS)
         write(6,*)' IRET FROM GETGB1S ',IRGS
         IF(IRGI .NE. 0) THEN
           WRITE(6,*)' PROBLEMS ON 1ST READ OF  FILE SO ABORT'
           ISTAT = IRGS
           RETURN
         ENDIF
       ENDDO

C    GET GRID NUMBER FROM PDS AND PROCESS GRIB FILE
C
       IGDNUM4 = KPDS(3)
       IMAX = KGDS(2)
       JMAX = KGDS(3)
       NUMVAL4 = IMAX*JMAX
       KMAX = MAXLEV
       WRITE(6,281) IMAX,JMAX,NUMLEV,KMAX
      ENDIF

C GSM  READ TEMPERATURE FILES FOR 12-HR MIN/MAX AT 00/12Z valid times
C  READ INDEX FILE TO GET GRID SPECS
C
      IF (IFHR .EQ. 18 .OR. IFHR .EQ. 30 .OR. IFHR .EQ. 42 .OR.
     x  IFHR .EQ. 54 .OR. IFHR .EQ. 66 .OR. IFHR .EQ. 78) THEN
       IRGI = 1
       IRGS = 1
       KMAX = 0
       JR=0
       KSKIP = 0
      WRITE(FNAME(6:7),FMT='(I2)')LUGT1

       CALL BAOPEN(LUGT1,FNAME,IRETGB)
      WRITE(FNAME(6:7),FMT='(I2)')LUGT1I
       CALL BAOPEN(LUGT1I,FNAME,IRETGI)
       CALL GETGI(LUGT1I,KSKIP,MBUF,CBUF,NLEN,NNUM,IRGI)
       write(6,*)' IRET FROM GETGI ',IRGI
       IF(IRGI .NE. 0) THEN
         WRITE(6,*)' PROBLEMS READING GRIB INDEX FILE SO ABORT'
         ISTAT = IRGI
       ENDIF

       DO K = 1, NNUM
         JR = K - 1
         JPDS = -1
         JGDS = -1
         CALL GETGB1S(CBUF,NLEN,NNUM,JR,JPDS,JGDS,JENS,
     &               KR,KPDS,KGDS,KENS,LSKIP,LGRIB,IRGS)
         write(6,*)' IRET FROM GETGB1S ',IRGS
         IF(IRGI .NE. 0) THEN
           WRITE(6,*)' PROBLEMS ON 1ST READ OF  FILE SO ABORT'
           ISTAT = IRGS
           RETURN
         ENDIF
       ENDDO
                                                                                      
C    GET GRID NUMBER FROM PDS AND PROCESS GRIB FILE
C     NOTE: WE'LL ASSUME THE GRID NUMBER IS THE SAME FOR
C     ALL OF THESE MIN/MAX FILES AND NOT DO THIS FOR EACH
       IGDNUMT = KPDS(3)
       IMAX = KGDS(2)
       JMAX = KGDS(3)
       NUMVALT = IMAX*JMAX
       KMAX = MAXLEV
       WRITE(6,281) IMAX,JMAX,NUMLEV,KMAX

       IRGI = 1
       IRGS = 1
       KMAX = 0
       JR=0
       KSKIP = 0
      WRITE(FNAME(6:7),FMT='(I2)')LUGT2

       CALL BAOPEN(LUGT2,FNAME,IRETGB)
      WRITE(FNAME(6:7),FMT='(I2)')LUGT2I
       CALL BAOPEN(LUGT2I,FNAME,IRETGI)
       CALL GETGI(LUGT1I,KSKIP,MBUF,CBUF,NLEN,NNUM,IRGI)
       write(6,*)' IRET FROM GETGI ',IRGI
       IF(IRGI .NE. 0) THEN
         WRITE(6,*)' PROBLEMS READING GRIB INDEX FILE SO ABORT'
         ISTAT = IRGI
       ENDIF

       IRGI = 1
       IRGS = 1
       KMAX = 0
       JR=0
       KSKIP = 0
      WRITE(FNAME(6:7),FMT='(I2)')LUGT3

       CALL BAOPEN(LUGT3,FNAME,IRETGB)
      WRITE(FNAME(6:7),FMT='(I2)')LUGT3I
       CALL BAOPEN(LUGT3I,FNAME,IRETGI)
       CALL GETGI(LUGT3I,KSKIP,MBUF,CBUF,NLEN,NNUM,IRGI)
       write(6,*)' IRET FROM GETGI ',IRGI
       IF(IRGI .NE. 0) THEN
         WRITE(6,*)' PROBLEMS READING GRIB INDEX FILE SO ABORT'
         ISTAT = IRGI
       ENDIF

       IRGI = 1
       IRGS = 1
       KMAX = 0
       JR=0
       KSKIP = 0
      WRITE(FNAME(6:7),FMT='(I2)')LUGT4

       CALL BAOPEN(LUGT4,FNAME,IRETGB)
      WRITE(FNAME(6:7),FMT='(I2)')LUGT4I
       CALL BAOPEN(LUGT4I,FNAME,IRETGI)
       CALL GETGI(LUGT4I,KSKIP,MBUF,CBUF,NLEN,NNUM,IRGI)
       write(6,*)' IRET FROM GETGI ',IRGI
       IF(IRGI .NE. 0) THEN
         WRITE(6,*)' PROBLEMS READING GRIB INDEX FILE SO ABORT',LUGT4
         ISTAT = IRGI
       ENDIF

       IRGI = 1
       IRGS = 1
       KMAX = 0
       JR=0
       KSKIP = 0
       WRITE(FNAME(6:7),FMT='(I2)')LUGT5

       CALL BAOPEN(LUGT5,FNAME,IRETGB)
       WRITE(FNAME(6:7),FMT='(I2)')LUGT5I
       CALL BAOPEN(LUGT5I,FNAME,IRETGI)
       CALL GETGI(LUGT5I,KSKIP,MBUF,CBUF,NLEN,NNUM,IRGI)
       write(6,*)' IRET FROM GETGI ',IRGI
       IF(IRGI .NE. 0) THEN
         WRITE(6,*)' PROBLEMS READING GRIB INDEX FILE SO ABORT',LUGT5
         ISTAT = IRGI
       ENDIF
      ENDIF

c   to start each new file with its index, set J=-1 
C
      J = 0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 007
      JPDS(6) = 001
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, 'sfc hght KF K ', KF, K
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          ZSFC(M,N) = GRID(KK)
c          IF (ZSFC(M,N) .LT. 0.0) ZSFC(M,N)=0.0
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK EGRID FILE ', IRET
         ISTAT = IRET
       RETURN
      ENDIF

c get surface pressure
      J = 0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 001
      JPDS(6) = 001
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, 'sfc pres KF K ', KF, K
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          PSFC(M,N) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK EGRID FILE '
         ISTAT = IRET
       RETURN
      ENDIF

c get 4 precip types 
      J = 0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 143 
      JPDS(6) = 001
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, 'snow KF K ', KF, K
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          ISNOW(M,N) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK EGRID FILE '
         ISTAT = IRET
       RETURN
      ENDIF

      J = 0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 142
      JPDS(6) = 001
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, 'ip KF K ', KF, K
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          IIP(M,N) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK EGRID FILE '
         ISTAT = IRET
       RETURN
      ENDIF

c frz rain
      J = 0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 141
      JPDS(6) = 001
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, 'zr KF K ', KF, K
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          IZR(M,N) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK EGRID FILE '
         ISTAT = IRET
       RETURN
      ENDIF

      J = 0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 140
      JPDS(6) = 001
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, 'rain KF K ', KF, K
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          IRAIN(M,N) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK EGRID FILE '
         ISTAT = IRET
       RETURN
      ENDIF

c lowest wet bulb zero level
      J = 0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 7 
      JPDS(6) = 245 
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, 'bulb KF K ', KF, K
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          WETFRZ(M,N) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK EGRID FILE '
         ISTAT = IRET
       RETURN
      ENDIF

c visibility 
      J = 0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 020
      JPDS(6) = 001
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, 'vis KF K ', KF, K
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          VIS(M,N) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK EGRID FILE '
         ISTAT = IRET
       RETURN
      ENDIF

c 2-m temp
      J = 0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 11 
      JPDS(6) = 105 
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, 'bulb KF K ', KF, K
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          T2(M,N) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK EGRID FILE '
         ISTAT = IRET
       RETURN
      ENDIF

c 2-m spec hum
      J = 0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 51 
      JPDS(6) = 105
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, '2q KF K ', KF, K
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          Q2(M,N) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK EGRID FILE '
         ISTAT = IRET
       RETURN
      ENDIF

c 2-m dew point 
      J = 0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 17 
      JPDS(6) = 105
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, '2dew KF K ', KF, K
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          D2(M,N) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK EGRID FILE '
         ISTAT = IRET
       RETURN
      ENDIF


c 10-m U
      J = 0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 33
      JPDS(6) = 105
      JPDS(7) = 10
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          U10(M,N) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK 10-M U '
         ISTAT = IRET
       RETURN
      ENDIF

c 10-m V
      J = 0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 34
      JPDS(6) = 105
      JPDS(7) = 10
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          V10(M,N) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK 10-M V '
         ISTAT = IRET
       RETURN
      ENDIF

c vegetation fraction
      J = 0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 225
      JPDS(6) = 001
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, 'veg KF K ', KF, K
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          VEG(M,N) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK VEG '
         ISTAT = IRET
       RETURN
      ENDIF

c Best Liftex Index 
      J = 0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 132 
      JPDS(6) = 116 
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, 'bli KF K ', KF, K
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          BLI(M,N) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK EGRID FILE '
         ISTAT = IRET
       RETURN
      ENDIF

c 6-hr Precip from special file
      IF(MOD(IFHR,6).EQ.0) THEN
       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM3
       JPDS(5) = 61
       JPDS(6) = 001
       print *, '6checks ', IGDNUM3, NUMVAL3, LUGP,LUGPI
       CALL GETGB(LUGP,LUGPI,NUMVAL3,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         print *, '6pcp KF K ', KF, K
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
             M=ILIM
             N=INT(KK/ILIM)
           ELSE
             M=MOD(KK,ILIM)
             N=INT(KK/ILIM) + 1
           ENDIF
           P06M(M,N) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK 6PRECIP FILE '
          ISTAT = IRET
       ENDIF

       J = 0
       JPDS = -1
       JPDS(3) = IGDNUMSN
       JPDS(5) = 65
       JPDS(6) = 001
       CALL GETGB(LUGS,LUGSI,NUMVALSN,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         print *, '6snow KF K ', KF, K
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
             M=ILIM
             N=INT(KK/ILIM)
           ELSE
             M=MOD(KK,ILIM)
             N=INT(KK/ILIM) + 1
           ENDIF
           SN06(M,N) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK 6SNOW FILE '
          ISTAT = IRET
       ENDIF
       ELSE
        DO J=1,JLIM
        DO I=1,ILIM
          P06M(I,J)=0.0
          SN06(I,J)=-99
        ENDDO
        ENDDO
      ENDIF

c 12-hr precip accumulation
      IF (IFHR .EQ. 18 .OR. IFHR .EQ. 30 .OR. IFHR .EQ. 42 .OR.
     x   IFHR .EQ. 54 .OR. IFHR .EQ. 66 .OR. IFHR .EQ. 78) THEN
       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM4
       JPDS(5) = 61
       JPDS(6) = 001
       print *, '12checks ', IGDNUM4, NUMVAL4, LUGP2,LUGP2I
       CALL GETGB(LUGP2,LUGP2I,NUMVAL4,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         print *, '12pcp KF K ', KF, K
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
             M=ILIM
             N=INT(KK/ILIM)
           ELSE
             M=MOD(KK,ILIM)
             N=INT(KK/ILIM) + 1
           ENDIF
           P12M(M,N) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK 12PRECIP FILE '
          ISTAT = IRET
       ENDIF
       print *, 'p12check ', P12M(150,300)
      ELSE
       DO J=1,JLIM
       DO I=1,ILIM
        P12M(I,J)=0.0
       ENDDO
       ENDDO
      ENDIF

c get the 3-hr precip out of the full grib file
c 3-hr Precip
       print *, 'standard 3-hr precip from reg file'
       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 61
       JPDS(6) = 001
       CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         print *, '3pcp KF K ', KF, K
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
             M=ILIM
             N=INT(KK/ILIM)
           ELSE
             M=MOD(KK,ILIM)
             N=INT(KK/ILIM) + 1
           ENDIF
           P03M(M,N) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK 3-HR PCP '
         ISTAT = IRET
       ENDIF
       print *, 'standard 3-hr snow from reg file'
       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 65
       JPDS(6) = 001
       CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         print *, '3snow KF K ', KF, K
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
             M=ILIM
             N=INT(KK/ILIM)
           ELSE
             M=MOD(KK,ILIM)
             N=INT(KK/ILIM) + 1
           ENDIF
           SN03(M,N) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK 3-HR SNOW '
         ISTAT = IRET
       ENDIF

      IF (IFHR .EQ. 18 .OR. IFHR .EQ. 30 .OR. IFHR .EQ. 42 .OR.
     x   IFHR .EQ. 54 .OR. IFHR .EQ. 66 .OR. IFHR .EQ. 78) THEN
c get min/max temperature values 
       print *, 'grabbing 12 hours of max/min data'
       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 11 
       JPDS(6) = 001
       CALL GETGB(LUGT1,LUGT1I,NUMVALT,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         print *, 'minmaxT2 KF K ', KF, K
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
             M=ILIM
             N=INT(KK/ILIM)
           ELSE
             M=MOD(KK,ILIM)
             N=INT(KK/ILIM) + 1
           ENDIF
           THOLD(M,N,2) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK THOLD2'
         ISTAT = IRET
       ENDIF

       J = 0 
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 17 
       JPDS(6) = 001
       CALL GETGB(LUGT1,LUGT1I,NUMVALT,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         print *, 'minmaxD2 KF K ', KF, K
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
             M=ILIM
             N=INT(KK/ILIM)
           ELSE
             M=MOD(KK,ILIM)
             N=INT(KK/ILIM) + 1
           ENDIF
           DHOLD(M,N,2) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK DHOLD2'
         ISTAT = IRET
       ENDIF

       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 11
       JPDS(6) = 001
       CALL GETGB(LUGT2,LUGT2I,NUMVALT,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
             M=ILIM
             N=INT(KK/ILIM)
           ELSE
             M=MOD(KK,ILIM)
             N=INT(KK/ILIM) + 1
           ENDIF
           THOLD(M,N,3) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK THOLD3'
         ISTAT = IRET
       ENDIF

       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 17
       JPDS(6) = 001
       CALL GETGB(LUGT2,LUGT2I,NUMVALT,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
             M=ILIM
             N=INT(KK/ILIM)
           ELSE
             M=MOD(KK,ILIM)
             N=INT(KK/ILIM) + 1
           ENDIF
           DHOLD(M,N,3) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK DHOLD3'
         ISTAT = IRET
       ENDIF

       IFHR4=IFHR-3
       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 11
       JPDS(6) = 001
       JPDS(14) = IFHR4
       CALL GETGB(LUGT3,LUGT3I,NUMVALT,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
             M=ILIM
             N=INT(KK/ILIM)
           ELSE
             M=MOD(KK,ILIM)
             N=INT(KK/ILIM) + 1
           ENDIF
           THOLD(M,N,4) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK THOLD4'
         ISTAT = IRET
       ENDIF

       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 17
       JPDS(6) = 001
       JPDS(14) = IFHR4
       CALL GETGB(LUGT3,LUGT3I,NUMVALT,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
             M=ILIM
             N=INT(KK/ILIM)
           ELSE
             M=MOD(KK,ILIM)
             N=INT(KK/ILIM) + 1
           ENDIF
           DHOLD(M,N,4) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK DHOLD4'
         ISTAT = IRET
       ENDIF

       IFHR5=IFHR-4
       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 11
       JPDS(6) = 001
       JPDS(14) = IFHR5
       CALL GETGB(LUGT3,LUGT3I,NUMVALT,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
             M=ILIM
             N=INT(KK/ILIM)
           ELSE
             M=MOD(KK,ILIM)
             N=INT(KK/ILIM) + 1
           ENDIF
           THOLD(M,N,5) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK THOLD5'
         ISTAT = IRET
       ENDIF

       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 17
       JPDS(6) = 001
       JPDS(14) = IFHR5
       CALL GETGB(LUGT3,LUGT3I,NUMVALT,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
             M=ILIM
             N=INT(KK/ILIM)
           ELSE
             M=MOD(KK,ILIM)
             N=INT(KK/ILIM) + 1
           ENDIF
           DHOLD(M,N,5) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK DHOLD5'
         ISTAT = IRET
       ENDIF

       IFHR6=IFHR-5
       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 11
       JPDS(6) = 001
       JPDS(14) = IFHR6
       CALL GETGB(LUGT3,LUGT3I,NUMVALT,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
             M=ILIM
             N=INT(KK/ILIM)
           ELSE
             M=MOD(KK,ILIM)
             N=INT(KK/ILIM) + 1
           ENDIF
           THOLD(M,N,6) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK THOLD6'
         ISTAT = IRET
       ENDIF

       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 17
       JPDS(6) = 001
       JPDS(14) = IFHR6
       CALL GETGB(LUGT3,LUGT3I,NUMVALT,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
             M=ILIM
             N=INT(KK/ILIM)
           ELSE
             M=MOD(KK,ILIM)
             N=INT(KK/ILIM) + 1
           ENDIF
           DHOLD(M,N,6) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK DHOLD6'
         ISTAT = IRET
       ENDIF

       IFHR7=IFHR-6
       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 11
       JPDS(6) = 001
       JPDS(14) = IFHR7
       CALL GETGB(LUGT4,LUGT4I,NUMVALT,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
             M=ILIM
             N=INT(KK/ILIM)
           ELSE
             M=MOD(KK,ILIM)
             N=INT(KK/ILIM) + 1
           ENDIF
           THOLD(M,N,7) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK THOLD7'
         ISTAT = IRET
       ENDIF

       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 17
       JPDS(6) = 001
       JPDS(14) = IFHR7
       CALL GETGB(LUGT4,LUGT4I,NUMVALT,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
             M=ILIM
             N=INT(KK/ILIM)
           ELSE
             M=MOD(KK,ILIM)
             N=INT(KK/ILIM) + 1
           ENDIF
           DHOLD(M,N,7) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK DHOLD7'
         ISTAT = IRET
       ENDIF

       IFHR8=IFHR-7
       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 11
       JPDS(6) = 001
       JPDS(14) = IFHR8
       CALL GETGB(LUGT4,LUGT4I,NUMVALT,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
             M=ILIM
             N=INT(KK/ILIM)
           ELSE
             M=MOD(KK,ILIM)
             N=INT(KK/ILIM) + 1
           ENDIF
           THOLD(M,N,8) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK THOLD8'
         ISTAT = IRET
       ENDIF

       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 17
       JPDS(6) = 001
       JPDS(14) = IFHR8
       CALL GETGB(LUGT4,LUGT4I,NUMVALT,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
             M=ILIM
             N=INT(KK/ILIM)
           ELSE
             M=MOD(KK,ILIM)
             N=INT(KK/ILIM) + 1
           ENDIF
           DHOLD(M,N,8) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK DHOLD8'
         ISTAT = IRET
       ENDIF

       IFHR9=IFHR-8
       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 11
       JPDS(6) = 001
       JPDS(14) = IFHR9
       CALL GETGB(LUGT4,LUGT4I,NUMVALT,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
             M=ILIM
             N=INT(KK/ILIM)
           ELSE
             M=MOD(KK,ILIM)
             N=INT(KK/ILIM) + 1
           ENDIF
           THOLD(M,N,9) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK THOLD9'
         ISTAT = IRET
       ENDIF

       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 17
       JPDS(6) = 001
       JPDS(14) = IFHR9
       CALL GETGB(LUGT4,LUGT4I,NUMVALT,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
             M=ILIM
             N=INT(KK/ILIM)
           ELSE
             M=MOD(KK,ILIM)
             N=INT(KK/ILIM) + 1
           ENDIF
           DHOLD(M,N,9) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK DHOLD9'
         ISTAT = IRET
       ENDIF

       IFHR10=IFHR-9
       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 11
       JPDS(6) = 001
       JPDS(14) = IFHR10
       CALL GETGB(LUGT5,LUGT5I,NUMVALT,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
             M=ILIM
             N=INT(KK/ILIM)
           ELSE
             M=MOD(KK,ILIM)
             N=INT(KK/ILIM) + 1
           ENDIF
           THOLD(M,N,10) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK THOLD10'
         ISTAT = IRET
       ENDIF

       IFHR10=IFHR-9
       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 17
       JPDS(6) = 001
       JPDS(14) = IFHR10
       CALL GETGB(LUGT5,LUGT5I,NUMVALT,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
             M=ILIM
             N=INT(KK/ILIM)
           ELSE
             M=MOD(KK,ILIM)
             N=INT(KK/ILIM) + 1
           ENDIF
           DHOLD(M,N,10) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK DHOLD10'
         ISTAT = IRET
       ENDIF

       IFHR11=IFHR-10
       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 11
       JPDS(6) = 001
       JPDS(14) = IFHR11
       CALL GETGB(LUGT5,LUGT5I,NUMVALT,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
             M=ILIM
             N=INT(KK/ILIM)
           ELSE
             M=MOD(KK,ILIM)
             N=INT(KK/ILIM) + 1
           ENDIF
           THOLD(M,N,11) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK THOLD11'
         ISTAT = IRET
       ENDIF

       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 17
       JPDS(6) = 001
       JPDS(14) = IFHR11
       CALL GETGB(LUGT5,LUGT5I,NUMVALT,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
             M=ILIM
             N=INT(KK/ILIM)
           ELSE
             M=MOD(KK,ILIM)
             N=INT(KK/ILIM) + 1
           ENDIF
           DHOLD(M,N,11) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK DHOLD11'
         ISTAT = IRET
       ENDIF

       IFHR12=IFHR-11
       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 11
       JPDS(6) = 001
       JPDS(14) = IFHR12
       CALL GETGB(LUGT5,LUGT5I,NUMVALT,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
             M=ILIM
             N=INT(KK/ILIM)
           ELSE
             M=MOD(KK,ILIM)
             N=INT(KK/ILIM) + 1
           ENDIF
           THOLD(M,N,12) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK THOLD12'
         ISTAT = IRET
       ENDIF

       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 17
       JPDS(6) = 001
       JPDS(14) = IFHR12
       CALL GETGB(LUGT5,LUGT5I,NUMVALT,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
             M=ILIM
             N=INT(KK/ILIM)
           ELSE
             M=MOD(KK,ILIM)
             N=INT(KK/ILIM) + 1
           ENDIF
           DHOLD(M,N,12) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK DHOLD12'
         ISTAT = IRET
       ENDIF
 
      ELSE IF (MOD(IFHR,3).EQ.0.) THEN
c      get max/min data for previous 2 hours gsm
       IRGI = 1
       IRGS = 1
       KMAX = 0
       JR=0
       KSKIP = 0
      WRITE(FNAME(6:7),FMT='(I2)')LUGT1
       CALL BAOPEN(LUGT1,FNAME,IRETGB)
      WRITE(FNAME(6:7),FMT='(I2)')LUGT1I
       CALL BAOPEN(LUGT1I,FNAME,IRETGI)
       CALL GETGI(LUGT1I,KSKIP,MBUF,CBUF,NLEN,NNUM,IRGI)
       write(6,*)' IRET FROM GETGI ',IRGI
       IF(IRGI .NE. 0) THEN
         WRITE(6,*)' PROBLEMS READING GRIB INDEX FILE SO ABORT'
         ISTAT = IRGI
       ENDIF
        DO K = 1, NNUM
         JR = K - 1
         JPDS = -1
         JGDS = -1
         CALL GETGB1S(CBUF,NLEN,NNUM,JR,JPDS,JGDS,JENS,
     &               KR,KPDS,KGDS,KENS,LSKIP,LGRIB,IRGS)
         write(6,*)' IRET FROM GETGB1S ',IRGS
         IF(IRGI .NE. 0) THEN
           WRITE(6,*)' PROBLEMS ON 1ST READ OF  FILE SO ABORT'
           ISTAT = IRGS
           RETURN
         ENDIF
       ENDDO

C    GET GRID NUMBER FROM PDS AND PROCESS GRIB FILE
C     NOTE: WE'LL ASSUME THE GRID NUMBER IS THE SAME FOR
C     ALL OF THESE MIN/MAX FILES AND NOT DO THIS FOR EACH
       IGDNUMT = KPDS(3)
       IMAX = KGDS(2)
       JMAX = KGDS(3)
       NUMVALT = IMAX*JMAX
       KMAX = MAXLEV
       WRITE(6,281) IMAX,JMAX,NUMLEV,KMAX

       IRGI = 1
       IRGS = 1
       KMAX = 0
       JR=0
       KSKIP = 0
       WRITE(FNAME(6:7),FMT='(I2)')LUGT2

       CALL BAOPEN(LUGT2,FNAME,IRETGB)
       WRITE(FNAME(6:7),FMT='(I2)')LUGT2I
       CALL BAOPEN(LUGT2I,FNAME,IRETGI)
       CALL GETGI(LUGT1I,KSKIP,MBUF,CBUF,NLEN,NNUM,IRGI)
       write(6,*)' IRET FROM GETGI ',IRGI
       IF(IRGI .NE. 0) THEN
         WRITE(6,*)' PROBLEMS READING GRIB INDEX FILE SO ABORT'
         ISTAT = IRGI
       ENDIF

       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 11
       JPDS(6) = 001
       CALL GETGB(LUGT1,LUGT1I,NUMVALT,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
             M=ILIM
             N=INT(KK/ILIM)
           ELSE
             M=MOD(KK,ILIM)
             N=INT(KK/ILIM) + 1
           ENDIF
           THOLD(M,N,2) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK THOLD2'
         ISTAT = IRET
       ENDIF

       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 17
       JPDS(6) = 001
       CALL GETGB(LUGT1,LUGT1I,NUMVALT,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
             M=ILIM
             N=INT(KK/ILIM)
           ELSE
             M=MOD(KK,ILIM)
             N=INT(KK/ILIM) + 1
           ENDIF
           DHOLD(M,N,2) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK DHOLD2'
         ISTAT = IRET
       ENDIF

       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 11
       JPDS(6) = 001
       CALL GETGB(LUGT2,LUGT2I,NUMVALT,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
             M=ILIM
             N=INT(KK/ILIM)
           ELSE
             M=MOD(KK,ILIM)
             N=INT(KK/ILIM) + 1
           ENDIF
           THOLD(M,N,3) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK THOLD3'
         ISTAT = IRET
       ENDIF

       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 17
       JPDS(6) = 001
       CALL GETGB(LUGT2,LUGT2I,NUMVALT,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
             M=ILIM
             N=INT(KK/ILIM)
           ELSE
             M=MOD(KK,ILIM)
             N=INT(KK/ILIM) + 1
           ENDIF
           DHOLD(M,N,3) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK DHOLD3'
         ISTAT = IRET
       ENDIF
      ENDIF

c   get the vertical profile of pressure 
      J=0
      DO LL=1,MAXLEV
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 001
       JPDS(6) = 109
       CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       J=K
       IF(IRET.EQ.0) THEN
         print *, 'press LL  KF K  ', LL, KF, K
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
           PMID(M,N,LL) = GRID(KK)
         ENDDO
       ELSE
        WRITE(6,*)' COULD NOT UNPACK EGRID FILE '
         ISTAT = IRET
       ENDIF
      ENDDO

c   get the vertical profile of height 
      J=0
      DO LL=1,MAXLEV
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 007
       JPDS(6) = 109
       CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       J=K
       IF(IRET.EQ.0) THEN
         print *, 'hght LL  KF K  ', LL, KF, K
         DO KK = 1,ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
           ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
           ENDIF
           HGHT(M,N,LL) = GRID(KK)
         ENDDO
       ELSE
        WRITE(6,*)' COULD NOT UNPACK EGRID FILE '
         ISTAT = IRET
       ENDIF
      ENDDO

c   get the vertical profile of temperature
      J=0
      DO LL=1,MAXLEV
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 011
       JPDS(6) = 109 
       CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       J=K
       IF(IRET.EQ.0) THEN
         print *, 'tmp LL KF K ', LL, KF, K
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
           ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
           ENDIF
           T(M,N,LL) = GRID(KK)
         ENDDO
       ELSE
        WRITE(6,*)' COULD NOT UNPACK EGRID FILE '
         ISTAT = IRET
       ENDIF
      ENDDO

c   get the vertical profile of q
      J=0
      DO LL=1,MAXLEV
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 051
       JPDS(6) = 109
       CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       J=K
       IF(IRET.EQ.0) THEN
         print *, 'q L KF K ', LL, KF, K
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
           ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
           ENDIF
           Q(M,N,LL) = GRID(KK)
         ENDDO
       ELSE
        WRITE(6,*)' COULD NOT UNPACK EGRID FILE '
         ISTAT = IRET
         RETURN
       ENDIF
      ENDDO

c   get the vertical profile of u 
             J=0
      DO LL=1,MAXLEV
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 033
       JPDS(6) = 109
       CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       J=K
       IF(IRET.EQ.0) THEN
         print *, 'u L KF K ', LL, KF, K
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
           ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
           ENDIF
           UWND(M,N,LL) = GRID(KK)
         ENDDO
       ELSE
        WRITE(6,*)' COULD NOT UNPACK EGRID FILE '
         ISTAT = IRET
       ENDIF
      ENDDO

c   get the vertical profile of v
      J=0
      DO LL=1,MAXLEV
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 034
       JPDS(6) = 109
       CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       J=K
       IF(IRET.EQ.0) THEN
         print *, 'v L KF K ', LL, KF, K
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
           ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
           ENDIF
           VWND(M,N,LL) = GRID(KK)
         ENDDO
       ELSE
        WRITE(6,*)' COULD NOT UNPACK EGRID FILE '
         ISTAT = IRET
         RETURN
       ENDIF
      ENDDO

c   get the vertical profile of cloud fraction 
      J=0
      DO LL=1,MAXLEV
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 071
       JPDS(6) = 109
       CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       J=K
       IF(IRET.EQ.0) THEN
         print *, 'cfr L KF K ', LL, KF, K
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
           ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
           ENDIF
           CFR(M,N,LL) = GRID(KK)
         ENDDO
       ELSE
        WRITE(6,*)' COULD NOT UNPACK EGRID FILE '
         ISTAT = IRET
         RETURN
       ENDIF
      ENDDO

c   950 mb temperature
      J=0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 011
      JPDS(6) = 100
      JPDS(7) = 950
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, '950 tmp KF K ', KF, K
        DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
           ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
           ENDIF
          T950(M,N) = GRID(KK)
        ENDDO
      ELSE
       WRITE(6,*)' COULD NOT UNPACK 950 T '
       ISTAT = IRET
      ENDIF

c   850 mb temperature
      J=0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 011
      JPDS(6) = 100
      JPDS(7) = 850
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, '850 tmp KF K ', KF, K
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          T850(M,N) = GRID(KK)
        ENDDO
      ELSE
       WRITE(6,*)' COULD NOT UNPACK 850 T'
       ISTAT = IRET
      ENDIF

c   700 mb temperature
      J=0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 011
      JPDS(6) = 100
      JPDS(7) = 700
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, '700 tmp KF K ', KF, K
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          T700(M,N) = GRID(KK)
        ENDDO
      ELSE
       WRITE(6,*)' COULD NOT UNPACK 850 T'
       ISTAT = IRET 
      ENDIF

c   500 mb temperature
      J=0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 011
      JPDS(6) = 100
      JPDS(7) = 500
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, '500 tmp KF K ', KF, K
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          T500(M,N) = GRID(KK)
        ENDDO
      ELSE
       WRITE(6,*)' COULD NOT UNPACK 500 T'
       ISTAT = IRET
      ENDIF

c   850 mb RH
      J=0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 052
      JPDS(6) = 100
      JPDS(7) = 850
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, '850 rh KF K ', KF, K
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          RH850(M,N) = GRID(KK)
        ENDDO
      ELSE
       WRITE(6,*)' COULD NOT UNPACK 850 RH'
       ISTAT = IRET
      ENDIF

c   700 mb RH
      J=0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 052
      JPDS(6) = 100
      JPDS(7) = 700
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, '700 rh KF K ', KF, K
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          RH700(M,N) = GRID(KK)
        ENDDO
      ELSE
       WRITE(6,*)' COULD NOT UNPACK 700 RH'
       ISTAT = IRET
      ENDIF

c sfc wind gust
      J = 0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 180
      JPDS(6) = 001
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, 'gust KF K ', KF, K
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          GUST(M,N) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK GUST'
         ISTAT = IRET
       RETURN
      ENDIF

c composite reflectivity
      J = 0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 212
      JPDS(6) = 200
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          REFC(M,N) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK REFC'
         ISTAT = IRET
       RETURN
      ENDIF

c get SREF precip
c 3-hr probability of .01"
 
      J = 0
      JPDS = -1
      JPDS(3) = IGDNUM2
      JPDS(5) = 191 
      JPDS(6) = 001
      CALL GETGB(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, '3sref .01 pcp KF K ', KF, K
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          S3REF01(M,N) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK 3PCP 01'
        ISTAT = IRET
      ENDIF

c probability of .1"

      J = 2
      JPDS = -1
      JPDS(3) = IGDNUM2
      JPDS(5) = 191 
      JPDS(6) = 001
      CALL GETGB(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, '3sref .10 pcp KF K ', KF, K
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          S3REF10(M,N) = GRID(KK)
        ENDDO
      ELSE
       WRITE(6,*)' COULD NOT UNPACK 3PCP 10'
       ISTAT = IRET
       RETURN
      ENDIF

c probability of 0.5"
                                              
      J = 4
      JPDS = -1
      JPDS(3) = IGDNUM2
      JPDS(5) = 191
      JPDS(6) = 001
      CALL GETGB(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, '3sref 50 pcp KF K ', KF, K
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          S3REF50(M,N) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK 3PCP 50'
        ISTAT = IRET
       RETURN
      ENDIF

c fill 6 and 12-hr probs with 0 if FHR=3 
c fill 12-hr probs with 0 with FHR=6 or 9
      IF (IFHR .EQ. 3) THEN
        print *, 'FHR=3 so zero 6 and 12-hr probs'
        DO N=1,JLIM
        DO M=1,ILIM
          S6REF01(M,N) = 0.0 
          S6REF10(M,N) = 0.0 
          S6REF50(M,N) = 0.0
          S12REF01(M,N) = 0.0
          S12REF10(M,N) = 0.0
          S12REF50(M,N) = 0.0
        ENDDO
        ENDDO
        GOTO 246
      ENDIF
      
c 6-hr probability of 0.01"
      J = 6
      JPDS = -1
      JPDS(3) = IGDNUM2
      JPDS(5) = 191
      JPDS(6) = 001
      CALL GETGB(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, '6sref 01 pcp KF K ', KF, K
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          S6REF01(M,N) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK 6PCP 01'
        ISTAT = IRET
       RETURN
      ENDIF

c 6-hr probability of 0.1"
      J = 8
      JPDS = -1
      JPDS(3) = IGDNUM2
      JPDS(5) = 191
      JPDS(6) = 001
      CALL GETGB(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, '6sref 10 pcp KF K ', KF, K
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          S6REF10(M,N) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK 6PCP 10'
        ISTAT = IRET
       RETURN
      ENDIF

c 6-hr probability of 0.5"
      J = 10 
      JPDS = -1
      JPDS(3) = IGDNUM2
      JPDS(5) = 191
      JPDS(6) = 001
      CALL GETGB(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, '6sref 50 pcp KF K ', KF, K
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          S6REF50(M,N) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK 6PCP 50'
        ISTAT = IRET
       RETURN
      ENDIF

      IF (IFHR .EQ. 6 .OR. IFHR .EQ. 9) THEN
        print *, 'FHR=6 or 9 so 12-hr probs'
        DO N=1,JLIM
        DO M=1,ILIM
          S12REF01(M,N) = 0.0
          S12REF10(M,N) = 0.0
          S12REF50(M,N) = 0.0
        ENDDO
        ENDDO
        GOTO 246
      ENDIF

c 12-hr probability of 0.01"
       
      J = 12 
      JPDS = -1
      JPDS(3) = IGDNUM2
      JPDS(5) = 191
      JPDS(6) = 001
      CALL GETGB(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, '12sref 01 pcp KF K ', KF, K
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          S12REF01(M,N) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK 12PCP 01'
        ISTAT = IRET
       RETURN
      ENDIF

c 12-hr probability of 0.1"
                                                                                 
      J = 14
      JPDS = -1
      JPDS(3) = IGDNUM2
      JPDS(5) = 191
      JPDS(6) = 001
      CALL GETGB(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, '12sref 10 pcp KF K ', KF, K
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          S12REF10(M,N) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK 12PCP 10'
        ISTAT = IRET
       RETURN
      ENDIF

c 12-hr probability of 0.5"
                     
      J = 16
      JPDS = -1
      JPDS(3) = IGDNUM2
      JPDS(5) = 191
      JPDS(6) = 001
      CALL GETGB(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, '12sref 50 pcp KF K ', KF, K
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          S12REF50(M,N) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK 12PCP 50'
        ISTAT = IRET
       RETURN
      ENDIF

 246  CONTINUE
      RETURN 
      END
