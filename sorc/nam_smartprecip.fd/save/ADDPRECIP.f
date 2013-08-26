       PROGRAM ADDPRECIP6
C                .      .    .                                       .
C SUBPROGRAM:    ADDPRECIP12 
C   PRGMMR: MANIKIN        ORG: W/NP22     DATE:  07-03-07
C
C ABSTRACT: PRODUCES 12-HOUR TOTAL AND CONVECTIVE PRECIPITATION BUCKETS
C              AS WELL AS SNOWFALL ON THE ETA NATIVE GRID FOR SMARTINIT 
C
C PROGRAM HISTORY LOG:
C   07-03-07  GEOFF MANIKIN 
C
C REMARKS:

C ATTRIBUTES:
C   LANGUAGE: FORTRAN-90
C   MACHINE:  CRAY C-90
C$$$
      INCLUDE "parmnam"
      PARAMETER(ITOT=ILIM*JLIM)
      DIMENSION GRID(ITOT),DIFF(5)
      DIMENSION INCDAT(8),JNCDAT(8)
      INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)
      INTEGER LEVS(MAXLEV),IVAR(5)
      INTEGER FHR1, FHR2, FHR3
      LOGICAL*1 MASK(ITOT), MASK2(ITOT)
C
      PARAMETER(MBUF=2000000,JF=1000000)
      CHARACTER CBUF(MBUF)
      CHARACTER CBUF2(MBUF)
      CHARACTER*11 ENVVAR
      CHARACTER*80 FNAME
      LOGICAL*1 LB(JF)
      REAL F(JF)
      PARAMETER(MSK1=32000,MSK2=4000)
      INTEGER JENS(200),KENS(200)
      DIMENSION APCP1(ITOT),APCP2(ITOT),APCP3(ITOT),
     &     APCP4(ITOT),APCPOUT(ITOT)
      DIMENSION CAPCP1(ITOT),CAPCP2(ITOT),CAPCP3(ITOT),
     &     CAPCP4(ITOT),CAPCPOUT(ITOT)
      DIMENSION SNOW1(ITOT),SNOW2(ITOT),SNOW3(ITOT),
     &     SNOW4(ITOT),SNOWOUT(ITOT)

CJTM  DIMENSION APCP(ITOT), APCP2(ITOT),APCP6HR(ITOT),
CJTM &     CAPCP(ITOT),CAPCP2(ITOT),CAPCP6HR(ITOT),
CJTM &     SNOW(ITOT),SNOW2(ITOT),SNOW6HR(ITOT)
C
Cjtm  Input Filename prefix on WCOSS
      FNAME='fort.  '

C     FHR3 = -99 signals a 6 hour summation requested
C     FHR4 GT 00 signals a 12 hour summation requested
      READ (5,*) FHR1, FHR2,FHR3,FHR4

      NUMLEV=MAXLEV
      if {fhr3 .lt. 0) FHR3=FHR1-3
      if {fhr4 .gt. 0) FHR0=FHR1-3
      print *, 'fhr1 fhr2 fhr3 fhr4 ', FHR1, FHR2, FHR3,FHR4
C
      LUGB=13;LUGI=14; LUGB2=15;LUGI2=16
      LUGB3=17;LUGI3=18;LUGB4=19;LUGI4=20
      LUGB5=50; LUGB6=51; LUGB5=52
C
      ISTAT = 0

C=======================================================
C  READ INDEX FILE TO GET GRID SPECS 
C=======================================================
      CALL RDHDRS(LUGB,LUGI,JPDS,JGDS,CBUF,IGDNUM,IMAX,JMAX,KMAX,NUMVAL)

C -== GET SURFACE FIELDS ==-

C   PRECIP 
      J = 0;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 061;JPDS(6) = 001
      JPDS(13) = 1
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,
     X           K,KPDS,KGDS,MASK,GRID,APCP1,IRET,ISTAT)

C  CONVECTIVE PRECIP
      J = 0;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 063;JPDS(6) = 001
      JPDS(13) = 1
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,
     X           K,KPDS,KGDS,MASK,GRID,CAPCP1,IRET,ISTAT)

C  SNOWFALL 
      J = 0;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 065;JPDS(6) = 001
      JPDS(14) = FHR3
      if (fhr4.gt.0) JPDS(14)=FHR0
      JPDS(15) = FHR1
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,
     X           K,KPDS,KGDS,MASK,GRID,SNOW1,IRET,ISTAT)

C=======================================================
C  READ INDEX FILE TO GET GRID SPECS for 2nd file
C=======================================================
      CALL RDHDRS(LUGB2,LUGI2,JPDS,JGDS,CBUF2,
     X            IGDNUM,IMAX,JMAX,KMAX,NUMVAL)

C -== GET SURFACE FIELDS ==-

C     ACCUMULATED PRECIP 
      J = -1;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 061;JPDS(6) = 001
      JPDS(13) = 1
      CALL SETVAR(LUGB2,LUGI2,NUMVAL,J,JPDS,JGDS,KF,
     X           K,KPDS,KGDS,MASK2,GRID,APCP2,IRET,ISTAT)

C     ACCUMULATED CONVECTIVE PRECIP
      J = -1;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 063;JPDS(6) = 001
      JPDS(13) = 1
      CALL SETVAR(LUGB2,LUGI2,NUMVAL,J,JPDS,JGDS,KF,
     X           K,KPDS,KGDS,MASK2,GRID,CAPCP2,IRET,ISTAT)

C     SNOWFALL
      J = -1;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 065;JPDS(6) = 001
      JPDS(14) = FHR1
      JPDS(15) = FHR2
      CALL SETVAR(LUGB2,LUGI2,NUMVAL,J,JPDS,JGDS,KF,
     X           K,KPDS,KGDS,MASK2,GRID,SNOW2,IRET,ISTAT)

      IF (FHR4.GT.0 ) THEN

C=======================================================
C  READ INDEX FILE TO GET GRID SPECS for 3rd file
C=======================================================
      CALL RDHDRS(LUGB3,LUGI3,JPDS,JGDS,CBUF2,
     X            IGDNUM,IMAX,JMAX,KMAX,NUMVAL)

C     ACCUMULATED PRECIP 
      J = 1;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 061;JPDS(6) = 001
      JPDS(13) = 1
      CALL SETVAR(LUGB3,LUGI3,NUMVAL,J,JPDS,JGDS,KF,
     X           K,KPDS,KGDS,MASK2,GRID,APCP3,IRET,ISTAT)

C     ACCUMULATED CONVECTIVE PRECIP
      J = -1;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 063;JPDS(6) = 001
      JPDS(13) = 1
      CALL SETVAR(LUGB3,LUGI3,NUMVAL,J,JPDS,JGDS,KF,
     X           K,KPDS,KGDS,MASK2,GRID,CAPCP3,IRET,ISTAT)

C     SNOWFALL
      J = -1;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 065;JPDS(6) = 001
        JPDS(14) = FHR2
        JPDS(15) = FHR3
      CALL SETVAR(LUGB3,LUGI3,NUMVAL,J,JPDS,JGDS,KF,
     X           K,KPDS,KGDS,MASK2,GRID,SNOW3,IRET,ISTAT)

C=======================================================
C  READ INDEX FILE TO GET GRID SPECS for 4th file
C=======================================================
      CALL RDHDRS(LUGB4,LUGI4,JPDS,JGDS,CBUF2,
     X            IGDNUM,IMAX,JMAX,KMAX,NUMVAL)

C     ACCUMULATED PRECIP 
      J = 1;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 061;JPDS(6) = 001
      JPDS(13) = 1
      CALL SETVAR(LUGB4,LUGI4,NUMVAL,J,JPDS,JGDS,KF,
     X           K,KPDS,KGDS,MASK2,GRID,APCP4,IRET,ISTAT)

C     ACCUMULATED CONVECTIVE PRECIP
      J = -1;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 063;JPDS(6) = 001
      JPDS(13) = 1
      CALL SETVAR(LUGB4,LUGI4,NUMVAL,J,JPDS,JGDS,KF,
     X           K,KPDS,KGDS,MASK2,GRID,CAPCP4,IRET,ISTAT)

C     SNOWFALL
      J = -1;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 065;JPDS(6) = 001
        JPDS(14) = FHR3
        JPDS(15) = FHR4
      CALL SETVAR(LUGB4,LUGI4,NUMVAL,J,JPDS,JGDS,KF,
     X           K,KPDS,KGDS,MASK2,GRID,SNOW4,IRET,ISTAT)

      ENDIF 

C=======================================================
C      OUTPUT 6 or 12 hr PRECIP BUCKETS
C=======================================================
       APCPOUT=APCP2+APCP1
       CAPCPOUT=CAPCP2+CAPCP1
       SNOWOUT=SNOW2R+ SNOW1
       IF (FHR4 .GT.0 )THEN
          APCPOUT=APCPOUT+APCP3+APCP4
          CAPCPOUT=CAPCPOUT+CAPCP3+CAPCP4
          SNOWOUT=SNOWOUT+SNOW3+SNOW4
       ENDIF

      KPDS(14)=FHR3
      KPDS(15)=FHR2
      IF (FHR4. GT.0 )THEN
        KPDS(14)=FHR0
        KPDS(15)=FHR4
      ENDIF

      KPDS(5)=61
      WRITE(FNAME(6:7),FMT='(I2)')LUGB5
      CALL BAOPEN(LUGB5,FNAME,IRETGB)
      CALL PUTGB(LUGB5,ITOT,KPDS,KGDS,MASK2,APCPOUT,IRET)
      CALL BACLOSE(LUGB5,IRET)

      KPDS(5)=63
      WRITE(FNAME(6:7),FMT='(I2)')LUGB6
      CALL BAOPEN(LUGB6,FNAME,IRET)
      CALL PUTGB(LUGB6,ITOT,KPDS,KGDS,MASK2,CAPCPOUT,IRET)
      CALL BACLOSE(LUGB6,IRET)

      KPDS(5)=65
      WRITE(FNAME(6:7),FMT='(I2)')LUGB7
      CALL BAOPEN(LUGB7,FNAME,IRET)
      CALL PUTGB(LUGB7,ITOT,KPDS,KGDS,MASK2,SNOWOUT,IRET)
      CALL BACLOSE(LUGB7,IRET)
      STOP
      END

      SUBROUTINE SETVAR(LUB,LUI,NUMV,J,JPDS,JGDS,KF,
     X           K,KPDS,KGDS,MASK,GRID,VARB,IRET,ISTAT)
C============================================================================
C     This Routine reads in a grib field and initializes a 2-D variable
C     Requested from w3lib GETGRB routine
C     10-2012   Jeff McQueen
C     NOTE: ONLY WORKS for REAL Type Variables
C============================================================================
C
C$$$
      INCLUDE "parmnam"
      PARAMETER(ITOT=ILIM*JLIM)
      DIMENSION GRID(ITOT),VARB(ITOT)
      INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)
      LOGICAL*1 MASK(ITOT)

C     Get GRIB Variable
      CALL GETGB(LUB,LUI,NUMV,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        DO KK = 1, ITOT
          VARB(KK) = GRID(KK)
        ENDDO
        WRITE(6,100) JPDS(5),JPDS(6),JPDS(7),J,MAXVAL(VARB)
 100    FORMAT('VARB UNPACKED ', 4I7,G12.4)
      ELSE
        WRITE(6,*)'===================================================='
        WRITE(6,*)'COULD NOT UNPACK VARB',J,JPDS(3),JPDS(5),JPDS(6),IRET
        WRITE(6,*)'UNIT', LUB,LUI,NUMV
        WRITE(6,*)'===================================================='
        print *, 'JPDS', JPDS
        print *, 'JGDS', JGDS
        ISTAT = IRET
        STOP 99
      ENDIF

      RETURN
      END

      SUBROUTINE RDHDRS(LUB,LUI,JPDS,JGDS,CBF,IGDN,IMAX,JMAX,KMAX,NUMV)
C=============================================================
C     This Routine Reads GRIB index file and returns its contents
C     (GETGI)
C     Also reads GRIB index and grib file headers to
C     find a GRIB message and unpack pds/gds parameters (GETGB1S)
C
C     10-2012  Jeff McQueen
C=============================================================
C$$$
      INCLUDE "parmnam"
      PARAMETER(ITOT=ILIM*JLIM)
      INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)
C
      PARAMETER(MBUF=2000000)
      CHARACTER CBF(MBUF)
      CHARACTER*80 FNAME
      INTEGER JENS(200),KENS(200)

Cjtm  Input Filename prefix on WCOSS
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
      CALL GETGI(LUI,KSKIP,MBUF,CBF,NLEN,NNUM,IRGI)

      write(6,*)' IRET FROM GETGI ',IRGI,LUB,LUI,NLEN
      IF(IRGI .NE. 0) THEN
        WRITE(6,*)' PROBLEMS READING GRIB INDEX FILE SO ABORT'
        ISTAT = IRGI
        STOP 9
      ENDIF

       DO K = 1, NNUM
        JR = K - 1
        JPDS = -1
        JGDS = -1
        CALL GETGB1S(CBF,NLEN,NNUM,JR,JPDS,JGDS,JENS,
     &               KR,KPDS,KGDS,KENS,LSKIP,LGRIB,IRGS)
        IF(IRGI .NE. 0) THEN
          WRITE(6,*)' PROBLEMS ON 1ST READ OF GRIB FILE SO ABORT'
          ISTAT = IRGS
          STOP 10
        ENDIF
        IGDN = KPDS(3)
        IMAX = KGDS(2)
        JMAX = KGDS(3)
        NUMV = IMAX*JMAX
        KMAX = MAXLEV
C
      ENDDO
      WRITE(6,280) IGDN,JPDS(4),JPDS(5),IMAX,JMAX,KMAX
  280 FORMAT(' IGDN, IMAX,JMAX,KMAX ',6I5)
      RETURN
      END
