      SUBROUTINE GETGRIB_ANL(PSFC,ZSFC,PMID,HGHT,T,Q,UWND,VWND,
     X    CFR,T2,Q2,D2,U10,V10,VEG,BLI,WETFRZ,VIS,T950,T850,
     X    T700,T500,RH850,RH700,GUST,REFC,DATE,IFHR)
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
      DIMENSION ZSFC(ILIM,JLIM),T(ILIM,JLIM,MAXLEV),PSFC(ILIM,JLIM),
     x  Q(ILIM,JLIM,MAXLEV),PMID(ILIM,JLIM,MAXLEV),WETFRZ(ILIM,JLIM),
     x  UWND(ILIM,JLIM,MAXLEV),VWND(ILIM,JLIM,MAXLEV),VIS(ILIM,JLIM),
     x  T2(ILIM,JLIM),Q2(ILIM,JLIM),D2(ILIM,JLIM), 
     x  U10(ILIM,JLIM),V10(ILIM,JLIM),
     x  HGHT(ILIM,JLIM,MAXLEV),BLI(ILIM,JLIM),T950(ILIM,JLIM),
     x  T850(ILIM,JLIM),T700(ILIM,JLIM),T500(ILIM,JLIM),
     x  CFR(ILIM,JLIM,MAXLEV),RH850(ILIM,JLIM),RH700(ILIM,JLIM),
     x  VEG(ILIM,JLIM),GUST(ILIM,JLIM),REFC(ILIM,JLIM)
C
      NUMLEV=MAXLEV
      LUGB=11
      LUGI=12
      FNAME='fort.  '

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

c   get sfc height
      J = 0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 007
      JPDS(6) = 001
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
          ZSFC(M,N) = GRID(KK)
          IF (ZSFC(M,N).LT.0.0) ZSFC(M,N)=0.0
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

c lowest wet bulb zero level
      J = 0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 7 
      JPDS(6) = 245 
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
        WRITE(6,*)' COULD NOT UNPACK EGRID FILE '
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

      RETURN 
      END
