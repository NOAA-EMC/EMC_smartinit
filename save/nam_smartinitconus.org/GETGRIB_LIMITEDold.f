      SUBROUTINE GETGRIB_LIMITED(PSFC,ZSFC,PMID,HGHT,T,Q,UWND,
     X         VWND,T2,Q2,D2,U10,V10,VEG,DATE)
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
      INTEGER LEVS(MAXLEV),IVAR(5),YEAR,MON,DAY,IHR,DATE
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
      PARAMETER(MSK1=32000,MSK2=1500)
      INTEGER JENS(200),KENS(200)
      DIMENSION T(ILIM,JLIM,MAXLEV),Q(ILIM,JLIM,MAXLEV),
     x  PMID(ILIM,JLIM,MAXLEV),HGHT(ILIM,JLIM,MAXLEV),
     x  UWND(ILIM,JLIM,MAXLEV),VWND(ILIM,JLIM,MAXLEV),
     x  T2(ILIM,JLIM),Q2(ILIM,JLIM),D2(ILIM,JLIM),
     x  U10(ILIM,JLIM),V10(ILIM,JLIM),
     x  ZSFC(ILIM,JLIM),PSFC(ILIM,JLIM),VEG(ILIM,JLIM)
C
      NUMLEV=MAXLEV
C
      LUGB=11
      LUGI=12
      LUGB2=13
      LUGI2=14
      LUGP=15
      LUGPI=16

      OPEN(17,file='DATE',form='formatted')
      READ(17,200) DUM1,DATE
      CLOSE(17)
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

Cjtm  Changed to use "fort." for input file names
      FNAME='fort.  '
      WRITE(FNAME(6:7),FMT='(I2)')LUGB
      CALL BAOPEN(LUGB,FNAME,IRETGB)
      write(6,*)' IRET FROM GETGB ',IRETGB

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
        IF(IRGI .NE. 0) THEN
          WRITE(6,*)' PROBLEMS ON 1ST READ OF GRIB FILE SO ABORT'
          ISTAT = IRGS
          RETURN
        ENDIF
C
      ENDDO
      write(6,*)' IRET FROM GETGB1S ',IRGS,' UNIT ', LUGB

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
        print *, 'sfc height KF K ', KF, K
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
        WRITE(6,*)' COULD NOT UNPACK SFC HEIGHT ', IRET
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
        WRITE(6,*)' COULD NOT UNPACK SURFACE PRESSURE '
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
        WRITE(6,*)' COULD NOT UNPACK 2-M T'
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
        print *, '2q KF K ITOT', KF, K, ITOT
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
        WRITE(6,*)' COULD NOT UNPACK 2-M Q'
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
        print *, '2d KF K ITOT', KF, K, ITOT
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
        WRITE(6,*)' COULD NOT UNPACK 2-M DEW'
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
        WRITE(6,*)' COULD NOT UNPACK GEOPOTENTIAL LVL ',LL
        ISTAT = IRET
        DO NN=1,JLIM
        DO MM=1,ILIM
         HGHT(MM,NN,LL)=-9999.
        ENDDO
        ENDDO
       ENDIF
      ENDDO

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
         DO KK = 1,ITOT
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
        WRITE(6,*)' COULD NOT UNPACK PRESSURE LVL ', LL
        ISTAT = IRET
        DO NN=1,JLIM
        DO MM=1,ILIM
         PMID(MM,NN,LL)=-9999.
        ENDDO
        ENDDO

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
        WRITE(6,*)' COULD NOT UNPACK TEMPERATURE LVL ', LL
        ISTAT = IRET
        DO NN=1,JLIM
        DO MM=1,ILIM
         T(MM,NN,LL)=-9999.
        ENDDO
        ENDDO
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
        WRITE(6,*)' COULD NOT UNPACK SPEC HUM LVL ', LL
        ISTAT = IRET
        DO NN=1,JLIM
        DO MM=1,ILIM
         Q(MM,NN,LL)=-9999.
        ENDDO
        ENDDO
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
        WRITE(6,*)' COULD NOT UNPACK UWND LVL ', LL 
        ISTAT = IRET
        DO NN=1,JLIM
        DO MM=1,ILIM
         UWND(MM,NN,LL)=-9999.
        ENDDO
        ENDDO
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
        WRITE(6,*)' COULD NOT UNPACK VWND LVL ', LL
        ISTAT = IRET
        DO NN=1,JLIM
        DO MM=1,ILIM
         VWND(MM,NN,LL)=-9999.
        ENDDO
        ENDDO
       ENDIF
      ENDDO
      RETURN 
      END
