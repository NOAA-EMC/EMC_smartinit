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
      DIMENSION APCP(ITOT), APCP2(ITOT),APCP6HR(ITOT),
     &     CAPCP(ITOT),CAPCP2(ITOT),CAPCP6HR(ITOT),
     &     SNOW(ITOT),SNOW2(ITOT),SNOW6HR(ITOT)
C
Cjtm  Input Filename prefix on WCOSS
      FNAME='fort.  '

      READ (5,*) FHR1, FHR2
      NUMLEV=MAXLEV
      FHR3=FHR1-3
      print *, 'fhr1 fhr2 fhr3 ', FHR1, FHR2, FHR3
C
      LUGB=13;LUGI=14; LUGB2=15;LUGI2=16
      LUGB3=50; LUGB4=51; LUGB5=52
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
     X           K,KPDS,KGDS,MASK,GRID,APCP,IRET,ISTAT)

C  CONVECTIVE PRECIP
      J = 0;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 063;JPDS(6) = 001
      JPDS(13) = 1
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,
     X           K,KPDS,KGDS,MASK,GRID,CAPCP,IRET,ISTAT)

C  SNOWFALL 
      J = 0;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 065;JPDS(6) = 001
      JPDS(14) = FHR3
      JPDS(15) = FHR1
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,
     X           K,KPDS,KGDS,MASK,GRID,SNOW,IRET,ISTAT)

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

C=======================================================
C      OUTPUT PRECIP BUCKETS
C=======================================================
       DO K = 1, ITOT
          APCP6HR(K)=APCP2(K)+APCP(K)
          CAPCP6HR(K)=CAPCP2(K)+CAPCP(K)
          SNOW6HR(K)=SNOW2(K)+SNOW(K)
       ENDDO

      KPDS(5)=61
      KPDS(14)=FHR3
      KPDS(15)=FHR2
      WRITE(FNAME(6:7),FMT='(I2)')LUGB3
      CALL BAOPEN(LUGB3,FNAME,IRETGB)
      CALL PUTGB(LUGB3,ITOT,KPDS,KGDS,MASK2,APCP6HR,IRET)
      CALL BACLOSE(LUGB3,IRET)

      KPDS(5)=63
      KPDS(14)=FHR3
      KPDS(15)=FHR2
      WRITE(FNAME(6:7),FMT='(I2)')LUGB4
      CALL BAOPEN(LUGB4,FNAME,IRET)
      CALL PUTGB(LUGB4,ITOT,KPDS,KGDS,MASK2,CAPCP6HR,IRET)
      CALL BACLOSE(LUGB4,IRET)

      KPDS(5)=65
      KPDS(14)=FHR3
      KPDS(15)=FHR2
      WRITE(FNAME(6:7),FMT='(I2)')LUGB5
      CALL BAOPEN(LUGB5,FNAME,IRET)
      CALL PUTGB(LUGB5,ITOT,KPDS,KGDS,MASK2,SNOW6HR,IRET)
      CALL BACLOSE(LUGB5,IRET)
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
