C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C SUBPROGRAM:    SMARTINITCONUS    CREATES NDFD FILES 
C   PRGRMMR: MANIKIN           ORG: W/NP22     DATE: 07-08-06
C
C ABSTRACT:   THIS CODE TAKES NATIVE NAM FILES AND GENERATES
C          5 KM OUTPUT OVER CONUS CONTAINING NDFD ELEMENTS
C
C PROGRAM HISTORY LOG:
C   07-08-06  G MANIKIN  - COMPLETED ADAPTING CODE TO NAM 
C

      PARAMETER(IM=1073,JM=689,MAXLEV=60)
      PARAMETER(ITOT=IM*JM)
      PARAMETER (CAPA=0.28589641)
      INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)
      INTEGER DATE
      INTEGER P, PP, R, RR, S, SS, W, WW, X, XX
      LOGICAL*1 MASK(ITOT), MASK2(ITOT)
      LOGICAL RITEHD,NEED12
      CHARACTER *50 WXSTRING(IM,JM)
C
      PARAMETER(MBUF=2000000,JF=1000000)
      PARAMETER (A2=17.2693882,A3=273.16,A4=35.86,
     &  PQ0=379.90516,P1000=100000.0)

      DIMENSION ID(25)
      INTEGER ISNOW(IM,JM),IZR(IM,JM),IIP(IM,JM),IRAIN(IM,JM)
      INTEGER FHR,CYC,HOUR
      REAL HGHT(IM,JM,MAXLEV), T(IM,JM,MAXLEV),
     X   Q(IM,JM,MAXLEV),UWND(IM,JM,MAXLEV),VWND(IM,JM,MAXLEV),
     X   PSFC(IM,JM),PMID(IM,JM,MAXLEV),ZSFC(IM,JM),
     X   RH(IM,JM,MAXLEV),PBLMARK(IM,JM),BLR(IM,JM),
     X   T950(IM,JM),T850(IM,JM),T700(IM,JM),T500(IM,JM),
     X   RH850(IM,JM),RH700(IM,JM),U10(IM,JM),V10(IM,JM),
     X   T2(IM,JM),Q2(IM,JM),BLI(IM,JM),REFC(IM,JM),
     X   QPF3(IM,JM),P03M(IM,JM),QPF6(IM,JM),P06M(IM,JM),
     X   QPF12(IM,JM),P12M(IM,JM),SN03(IM,JM),SN06(IM,JM),
     X   POP3(IM,JM),POP6(IM,JM),POP12(IM,JM),SNOWAMT3(IM,JM),
     X   SNOWAMT6(IM,JM),CWR(IM,JM),SKY(IM,JM),WETFRZ(IM,JM),
     X   DIRTRANS(IM,JM),T1(IM,JM),D2(IM,JM),CFR(IM,JM,MAXLEV),
     X   WX(IM,JM),THUNDER(IM,JM),VIS(IM,JM),GUST(IM,JM),
     X   P3CP01(IM,JM),P3CP10(IM,JM),P3CP50(IM,JM),
     X   P6CP01(IM,JM),P6CP10(IM,JM),P6CP50(IM,JM),
     X   P12CP01(IM,JM),P12CP10(IM,JM),P12CP50(IM,JM),
     X   TOPO(IM,JM),WGUST(IM,JM)
      REAL TOPO_NDFD(IM,JM),ROUGH(IM,JM),VEG(IM,JM) 
      REAL DOWNT(IM,JM),DOWNDEW(IM,JM),DOWNU(IM,JM),
     X   DOWNV(IM,JM),DOWNQ(IM,JM),GRIDWX(IM,JM)
      REAL THOLD(IM,JM,12),DHOLD(IM,JM,12),TMAX12(IM,JM),
     X   TMIN12(IM,JM),TMAX3(IM,JM),TMIN3(IM,JM),RHMAX12(IM,JM),
     X   RHMIN12(IM,JM),RHMAX3(IM,JM),RHMIN3(IM,JM),TH1(IM,JM),
     X   TH2(IM,JM),DH1(IM,JM),DH2(IM,JM)
      REAL MGTRANS(IM,JM),LAL(IM,JM),HAINES(IM,JM),
     X   MIXHGT(IM,JM)

      READ (5,*) FHR
      READ (5,*) CYC
      print *, 'into main ', FHR
      print *, 'cyc ', CYC
      FHR3=FHR-3
      FHR6=FHR-6
      FHR12=FHR-12

c  READ THE GRIB FILES FROM THE NAM AND SREF.  WE NEED TO READ A
c   FULL COMPLEMENT OF DATA EVERY 3 HOURS.  FOR THE IN-BETWEEN
c   FCST HOURS, WE ONLY NEED TO KEEP TRACK OF DOWNSCALED TEMP
c   AND DEW POINT (FOR MIN/MAX PURPOSES), SO WE NEED ONLY A VERY
c   LIMITED AMOUNT OF DATA.   FOR THE ANALYSIS TIME, WE NEED A
c   SPECIAL CALL OF THE FULL DATA SET BUT WITHOUT PRECIP

      IF (FHR .EQ. 0) THEN
       CALL GETGRIB_ANL(PSFC,ZSFC,PMID,HGHT,T,Q,UWND,VWND,
     X    CFR,T2,Q2,D2,U10,V10,VEG,BLI,WETFRZ,VIS,T950,T850,T700,
     X    T500,RH850,RH700,GUST,REFC,DATE,FHR)
       GOTO 222 
      ENDIF

      IF (CYC .EQ. 00 .OR. CYC .EQ. 12) THEN
       IF (MOD(FHR,3).EQ.0) THEN 
        CALL GETGRIB(PSFC,ZSFC,PMID,HGHT,T,Q,UWND,VWND,CFR,ISNOW,
     X    IZR,IIP,IRAIN,T2,Q2,D2,U10,V10,VEG,BLI,WETFRZ,VIS,T950,
     X    T850,T700,T500,RH850,RH700,GUST,REFC,P03M,P06M,P12M,SN03,
     X    SN06,P3CP01,P3CP10,P3CP50,P6CP01,P6CP10,P6CP50,P12CP01,
     X    P12CP10,P12CP50,THOLD,DHOLD,DATE,FHR)
       ELSE
        CALL GETGRIB_LIMITED(PSFC,ZSFC,PMID,HGHT,T,Q,UWND,VWND,T2,
     X     Q2,D2,U10,V10,VEG,DATE)
       ENDIF
      ELSE
       IF (MOD(FHR,3).EQ.0) THEN
        CALL GETGRIB_OFF(PSFC,ZSFC,PMID,HGHT,T,Q,UWND,VWND,CFR,
     X    ISNOW,IZR,IIP,IRAIN,T2,Q2,D2,U10,V10,VEG,BLI,WETFRZ,
     X    VIS,T950,T850,T700,T500,RH850,RH700,GUST,REFC,P03M,P06M,
     X    P12M,SN03,SN06,P3CP01,P3CP10,P3CP50,P6CP01,P6CP10,
     X    P6CP50,P12CP01,P12CP10,P12CP50,THOLD,
     X    DHOLD,DATE,FHR)
       ELSE
        CALL GETGRIB_LIMITED(PSFC,ZSFC,PMID,HGHT,T,Q,UWND,VWND,T2,
     X     Q2,D2,U10,V10,VEG,DATE)
       ENDIF
      ENDIF  
  
 222  CONTINUE
 
c  CALL THE DOWNSCALING CODE 

       CALL NDFDgrid(PSFC,ZSFC,T,HGHT,Q,UWND,VWND,PMID,T2,Q2,D2,
     X      U10,V10,VEG,DOWNT,DOWNDEW,DOWNU,DOWNV,DOWNQ,TOPO)

      IF (MOD(FHR,3).EQ.0) THEN 
        print *, 'into main 3-hr block'
       RITEHD = .TRUE.
       ID(1:25) = 0
       ID(8)=11
       ID(9)=1
       DEC=-2.0
       CALL GRIBIT(ID,RITEHD,DOWNT,DATE,FHR,70,DEC)

       ID(1:25) = 0
       ID(8)=17
       ID(9)=1
       DEC=-2.0
       CALL GRIBIT(ID,RITEHD,DOWNDEW,DATE,FHR,70,DEC)

       ID(1:25) = 0
       ID(8)=51
       ID(9)=1
       DEC=3.0
       CALL GRIBIT(ID,RITEHD,DOWNQ,DATE,FHR,70,DEC)

       ID(1:25) = 0
       ID(8)=33
       ID(9)=1
       DEC=-2.0
       CALL GRIBIT(ID,RITEHD,DOWNU,DATE,FHR,70,DEC)

       ID(1:25) = 0
       ID(8)=34
       ID(9)=1
       DEC=-2.0
       CALL GRIBIT(ID,RITEHD,DOWNV,DATE,FHR,70,DEC)

       DO J=1,JM
       DO I=1,IM
         SPEED=SQRT(DOWNU(I,J)*DOWNU(I,J)+DOWNV(I,J)*DOWNV(I,J))
         WGUST(I,J)=MAX(GUST(I,J),SPEED)
       ENDDO
       ENDDO

       ID(1:25) = 0
       ID(8)=180
       ID(9)=1
       DEC=3.0 
       CALL GRIBIT(ID,RITEHD,WGUST,DATE,FHR,70,DEC)

       ID(1:25) = 0
       ID(8)=1
       ID(9)=1
       DEC=3.0
       CALL GRIBIT(ID,RITEHD,PSFC,DATE,FHR,70,DEC)

C GSM  for boundary layer computations, find the number of
C       levels within the lowest 180 mb

       DO J=1,JM
       DO I=1,IM
         PBLMARK(I,J)=1
         TOP=PSFC(I,J)-18000.
         DO L=MAXLEV,1,-1
          IF(PMID(I,J,L).GT.TOP)THEN
           PBLMARK(I,J)=L
           GOTO 60
          ENDIF 
         ENDDO
 60     CONTINUE
       ENDDO
       ENDDO

c  compute RH
       DO J=1,JM
       DO I=1,IM
         DO L=1,MAXLEV
          QC=PQ0/PMID(I,J,L)
     1          *EXP(A2*(T(I,J,L)-A3)/(T(I,J,L)-A4))
         RH(I,J,L)=Q(I,J,L)/QC
         ENDDO
       ENDDO
       ENDDO

c  skip precip fields if FHR=0
       IF (FHR .EQ. 0) GOTO 444
c--------------------------------------------------------------------------
c QPF - simply take model QPF and change units to inches
c---------------------------------------- --------------------------------
  
        DO J = 1, JM
        DO I = 1, IM
         QPF3(I,J) = P03M(I,J) / 25.4   ! convert from millimeters to inches
         QPF6(I,J) = P06M(I,J) / 25.4
         QPF12(I,J) = P12M(I,J) / 25.4
c with interpolations and lack of precision in the code dealing
c  with buckets, 3-hr totals may occasionally end up slightly
c  larger than 6 or 12-hr totals, and we don't want that

         IF (QPF3(I,J) .GT. QPF12(I,J) .AND.
     x       QPF12(I,J) .NE. 0.0) THEN
               QPF3(I,J)=QPF12(I,J)
         ENDIF 
         IF (QPF3(I,J) .GT. QPF6(I,J) .AND.
     x       QPF6(I,J) .NE. 0.0) THEN
               QPF3(I,J)=QPF6(I,J)
         ENDIF 
        ENDDO
        ENDDO
c-------------------------------------------------------------------------
c PoP - based strongly on QPF (since when model has one inch of precip the
c   chance of getting 0.01 is pretty high).  However, there is a big
c   difference between a place that model has 0.00 precip and is very
c   close to precipitating - and those where model has 0.00 and is a
c   thousand miles from the nearest cloud.  Thus, uses the average
c   boundary layer RH to make an adjustment on the low end - adding
c   to PoP where RH is high.  Ignores surface RH to try to ignore fog
c  cases. Would also like to consider omega.
c
c   Uses hyperbolic tangent of QPF, so that it rises quickly as model
c   QPF increases - but tapers out to nearly 100% as QPF gets high.
c   Also uses hyperbolic tangent of QPF to reduce the impact of high RH
c   as QPF gets higher (since avg RH will always be high when QPF is high)
c
c   Adjustable parameters:
c     QPFMAX is QPF amount that would give 75% PoP if nothing else
c       considered at half this amount, PoP is 45%, at double this
c       amount PoP is 96%.  Default set at 0.40.
c     RHexcess is amount of average BL RH above which PoP is adjusted
c       upward. Default set to 60%
c     adjAmount is maximum amount of adjustment if BL RH is
c       totally saturated. Default set to 30%
c
c GSM   The above discussion is the original PoP methodology of the smarinit
c     code.   The problem is that in old days of the Eta with its dry bias,
c     when the model was able to generate very heavy amounts, it usually did
c     so with good reason.   In the current era of the high-res WRF, you can
c     now get very localized bullseyes of heavy QPF (often convective) that
c     don't verify.   This leads to localized bullseyes of PoP.   To generate
c     a more realistic field, I introduce PoP from the SREF and blend it with
c     the smartinit method to maintain some continuity with the NAM QPF.

c--------------------------------------------------------------------------
                                                                          
        QPFMAX=0.40    ! QPF value where raw PoP would be 75%
        RHexcess=60.0  ! RH above this can add to PoP and below will subtract
        adjAmount=30.0 ! amount of adjustment allowed

c 3-hr POP
        DO I = 1, IM
        DO J = 1, JM
c  due to interpolation from coarse to fine grid, the 3-hr pop can end up
c   higher than the 12-hr pop at the same grid point.  Even it out if this occurs.
         IF (FHR .GT. 11) THEN
          IF (P3CP01(I,J) .GT. P12CP01(I,J)) THEN
           TMPPCP=(P3CP01(I,J)+P12CP01(I,J))/2.
           P3CP01(I,J) = TMPPCP
           P12CP01(I,J) = TMPPCP
          ENDIF 

          IF (P3CP10(I,J) .GT. P12CP10(I,J)) THEN
            TMPPCP=(P3CP10(I,J)+P12CP10(I,J))/2.
            P3CP10(I,J) = TMPPCP
            P12CP10(I,J) = TMPPCP
          ENDIF 
         ENDIF

         factor=tanh(QPF3(I,J)/QPFMAX)
         factor2=tanh(QPF3(I,J)*(2.0/QPFMAX))
         rhsum=0
         LMBL=PBLMARK(I,J)
         DO L=1, LMBL
           RHSUM=RHSUM + RH(I,J,L)
         ENDDO
         RHAVG=100*RHSUM/LMBL
         rhmx=100-RHexcess
         dpop=rhavg-RHexcess
         IF(DPOP.LT.0.)DPOP=0.
         dpop=(dpop/rhmx)*(1.0-factor2)*adjAmount
         POPTMP3=(factor*100.0)+dpop
C GSM  changed pop3 for stable/convective environments
c       based on the best LI.   any NAM amounts in non-convective
c       regimes are generally more believable so do not need to
c       "verify" chances with SREF probs of higher amounts;  will
c       use higher SREF thresholds for convective regimes.
         IF (BLI(I,J).GT. 0.) THEN
           POP3(I,J)=(POPTMP3+P3CP01(I,J))/2.
         ELSE
          IF (POPTMP3 .GT. 70.)  THEN
           POP3(I,J)=(POPTMP3+P3CP01(I,J)+P3CP10(I,J))/3.
          ELSE
           POP3(I,J)=(2*POPTMP3+2*P3CP01(I,J)+P3CP10(I,J))/5.
          ENDIF
         ENDIF

        ENDDO
        ENDDO

        CALL BOUND(POP3,0.,100.)
        ID(1:25) = 0
        ID(8)=193
        ID(9)=1
        ID(18)=FHR3
        ID(19)=FHR
        ID(20)=4
        DEC=3.0
        CALL GRIBIT(ID,RITEHD,POP3,DATE,FHR,70,DEC)
            
        ID(1:25) = 0
        ID(8)=61
        ID(9)=1
        ID(18)=FHR3
        ID(19)=FHR
        ID(20)=4
        DEC=-3.0
        CALL GRIBIT(ID,RITEHD,P03M,DATE,FHR,70,DEC)

c 6-hr POP
       IF(MOD(FHR,6).EQ.0) THEN
        DO I = 1, IM
        DO J = 1, JM
         factor=tanh(QPF6(I,J)/QPFMAX)
         factor2=tanh(QPF6(I,J)*(2.0/QPFMAX))
         rhsum=0
         LMBL=PBLMARK(I,J)
         DO L=1, LMBL
           RHSUM=RHSUM + RH(I,J,L)
         ENDDO
         RHAVG=100*RHSUM/LMBL
         rhmx=100-RHexcess
         dpop=rhavg-RHexcess
         IF(DPOP.LT.0.)DPOP=0.
         dpop=(dpop/rhmx)*(1.0-factor2)*adjAmount
         POPTMP6=(factor*100.0)+dpop

c  GSM  not using the same test on liftex index as in 3-hr POP
c     as an instantaneous value doesn't reflect a 6-hr period
c     very well.  probably need to tie in convective precip at
c     some point.  very possible that POP6 can end up less than
c     POP3 since the computations are different - this isn't ideal,
c     but setting the POP6 to be no lower than the POP3 covers it

         IF (POPTMP6 .GT. 70.)  THEN
           POP6(I,J)=(2*POPTMP6+2*P6CP01(I,J)+P6CP10(I,J))/5.
         ELSE IF (POPTMP6 .LT. 30.) THEN
           POP6(I,J)=(POPTMP6+2*P6CP01(I,J)+P6CP10(I,J))/4.
         ELSE
           POP6(I,J)=AMAX1((POPTMP6+P6CP01(I,J))/2.,20.)
         ENDIF
         IF (POP6(I,J).LT.POP3(I,J)) THEN
            POP6(I,J)=POP3(I,J)
         ENDIF
        ENDDO
        ENDDO

        CALL BOUND(POP6,0.,100.)
        ID(1:25) = 0
        ID(8)=193
        ID(9)=1
        ID(18)=FHR6
        ID(19)=FHR
        ID(20)=4
        DEC=3.0
        CALL GRIBIT(ID,RITEHD,POP6,DATE,FHR,70,DEC)

        ID(1:25) = 0
        ID(8)=61
        ID(9)=1
        ID(18)=FHR6
        ID(19)=FHR
        ID(20)=4
        DEC=-3.0
        CALL GRIBIT(ID,RITEHD,P06M,DATE,FHR,70,DEC)
       ENDIF

c 12-hr POP
       IF (CYC .EQ. 00 .OR. CYC .EQ. 12) THEN
        IF (MOD(FHR,12).EQ.0) THEN
          NEED12=.TRUE.
        ELSE
          NEED12=.FALSE.
        ENDIF
       ELSE
        IF(MOD(FHR+6,12).EQ.0 .AND. FHR .NE. 6) THEN 
          NEED12=.TRUE.
        ELSE
          NEED12=.FALSE.
        ENDIF
       ENDIF

       IF(NEED12)THEN
        DO I = 1, IM
        DO J = 1, JM
         factor=tanh(QPF12(I,J)/QPFMAX)
         factor2=tanh(QPF12(I,J)*(2.0/QPFMAX))
         rhsum=0
         LMBL=PBLMARK(I,J)
         DO L=1, LMBL
           RHSUM=RHSUM + RH(I,J,L)
         ENDDO
         RHAVG=100*RHSUM/LMBL
         rhmx=100-RHexcess
         dpop=rhavg-RHexcess
         IF(DPOP.LT.0.)DPOP=0.
         dpop=(dpop/rhmx)*(1.0-factor2)*adjAmount
         POPTMP12=(factor*100.0)+dpop

c  GSM  not using the same test on liftex index as in 3-hr POP
c     as an instantaneous value doesn't reflect a 12-hr period
c     very well.  probably need to tie in convective precip at
c     some point.  very possible that POP12 can end up less than
c     POP3 since the computations are different - this isn't ideal,
c     but setting the POP12 to be no lower than the POP6 covers it

         IF (POPTMP12 .GT. 70.)  THEN
           POP12(I,J)=(POPTMP12+P12CP01(I,J)+P12CP10(I,J))/3.
         ELSE IF (POPTMP12 .LT. 30.) THEN
           POP12(I,J)=(2*POPTMP12+2*P12CP01(I,J)+P12CP10(I,J))/5.
         ELSE
           POP12(I,J)=AMAX1((POPTMP12+P12CP01(I,J))/2.,20.)
         ENDIF
         IF (POP12(I,J).LT.POP6(I,J)) THEN
          POP12(I,J)=POP6(I,J)
         ENDIF
        ENDDO
        ENDDO

        CALL BOUND(POP12,0.,100.)
        ID(1:25) = 0
        ID(8)=193
        ID(9)=1
        ID(18)=FHR12
        ID(19)=FHR
        ID(20)=4
        DEC=3.0
        CALL GRIBIT(ID,RITEHD,POP12,DATE,FHR,70,DEC)

        ID(1:25) = 0
        ID(8)=61
        ID(9)=1
        ID(18)=FHR12
        ID(19)=FHR
        ID(20)=4
        DEC=-3.0
        CALL GRIBIT(ID,RITEHD,P12M,DATE,FHR,70,DEC)
       ENDIF

      CALL MAKESTRING(IRAIN,ISNOW,IZR,IIP,BLI,POP3,WXSTRING,GRIDWX)

      ID(1:25) = 0
      ID(8)=140
      ID(9)=1
      DEC=3.0
      CALL GRIBIT(ID,RITEHD,GRIDWX,DATE,FHR,70,DEC)

c    #--------------------------------------------------------------------------
c    #  Chance of Wetting Rain (0.1 inch).  Same algorithm as PoP, but requires
c    #       more model QPF to get same chances, and higher boundary layer RH
c    #       to get the adjustment (and maximum adjustment is less).
c    #
c    #       Adjustable parameters:
c    #          QPFMAX should be higher than PoP topQPF
c    #                 Default set at 0.60.
c    #          RHexcess should be higher than PoP RHexcess
c    #                 Default set to 80%
c    #          adjAmount should be smaller than PoP adjAmount
c    #                 Default set to 10%
c    #
c    #--------------------------------------------------------------------------
        QPFMAX=0.60    ! QPF value where raw PoP would be 75%
        RHexcess=70.0  ! RH above this can add to PoP and below will subtract
        adjAmount=15.0 ! amount of adjustment allowed

        DO I = 1, IM
        DO J = 1, JM
         factor=tanh(QPF3(I,J)*(1.0/QPFMAX))
         factor2=tanh(QPF3(I,J)*(2.0/QPFMAX))
         rhsum=0
         LMBL=PBLMARK(I,J)
         DO L=1, LMBL
           RHSUM=RHSUM + RH(I,J,L)
         ENDDO
        RHAVG=RHSUM/LMBL
        rhmx=100-RHexcess
        DPOP=rhavg-RHexcess
        IF(DPOP.LT.0.) DPOP=0.
        dpop=(dpop/rhmx)*(1.0-factor2)*adjAmount
        TMPCWR=(factor*100.0)+dpop
        CWR(I,J)=(TMPCWR+2*P3CP10(I,J))/3.
        IF (QPF3(I,J).GT. 0.10) CWR(I,J)=AMAX1(CWR(I,J),30.)
        IF (CWR(I,J).GT.POP3(I,J)) CWR(I,J)=POP3(I,J)
        ENDDO
        ENDDO
        CALL BOUND(CWR,0.,100.)
        ID(1:25) = 0
        ID(2)=129
        ID(8)=130
        ID(9)=1
        ID(18)=FHR3
        ID(19)=FHR
        ID(20)=4
        DEC=3.0
        CALL GRIBIT(ID,RITEHD,CWR,DATE,FHR,70,DEC)

c===========================================================================
c  SnowAmt - simple snow ratio based on surface temperature - multiplied
c            times the model QPF amount
c GSM - using snow liquid directly from the model instead but still
c         constructing a snow ratio;  using averaged sfc temp over period
c---------------------------------------------------------------------------
        DO J=1,JM
        DO I=1,IM
         SNOWR=0
         IF (SN03(I,J).EQ.0.) GOTO 32
         TAVG=(DOWNT(I,J)+THOLD(I,J,2)+THOLD(I,J,3))/3.
         SNOWR=(273.15-TAVG)+8.
c convert to m
 32      SNOWAMT3(I,J)=SN03(I,J)*SNOWR*0.001
        ENDDO
        ENDDO

        ID(1:25) = 0
        ID(8)=66
        ID(9)=1
        ID(18)=FHR3
        ID(19)=FHR
        ID(20)=4
        DEC=3.0
        CALL GRIBIT(ID,RITEHD,SNOWAMT3,DATE,FHR,70,DEC)

        IF (MOD(FHR,6).EQ.0) THEN
         DO J=1,JM
         DO I=1,IM
          SNOWR=0
          IF (SN06(I,J).EQ.0.) GOTO 34
c  This needs to be updated eventually to use more temperature
c   data, but for now, it's the best guess at the avg temp
c   over the 6 hour period
          TAVG=(DOWNT(I,J)+THOLD(I,J,2)+2*THOLD(I,J,3))/4.
          IF (TAVG.LT.264.) THEN
           SNOWR=20
          ELSE
           SNOWR=((273.15-TAVG)/2)+8.
          ENDIF
c convert to m
 34       SNOWAMT6(I,J)=SN06(I,J)*SNOWR*0.001
         ENDDO
         ENDDO

         ID(1:25) = 0
         ID(8)=66
         ID(9)=1
         ID(18)=FHR6
         ID(19)=FHR
         ID(20)=4
         DEC=3.0
         CALL GRIBIT(ID,RITEHD,SNOWAMT6,DATE,FHR,70,DEC)
        ENDIF

 444    CONTINUE
c----------------------------------------------------------------
c  Sky - Calculates cloud percentage in each layer based on
c        RH in that layer.  Then adds up the percentages in
c        the layers. Model clouds seem too 'binary', and so
c        they are not used.
c
c        We guess that it takes higher RH near the surface (say
c        97%) to get a cloud, but less RH up high (say only 90%
c        to get cirrus).  Transition width is wider up high, than
c        it is near the surface.
c
c        Also weight high clouds less in the coverage than
c        low clouds.
c
c        No downscaling is attempted since an observer can usually
c        see MANY gridpoints - and judges clouds based on all of
c        them - not just whether there is a cloud in the small
c        gridpoint directly overhead.  Thus, cloud fields are
c        rather smooth.
c----------------------------------------------------------------

c  remove surface level - so surface Fog does not count

c          get weight based on pressure - high levels get counted little
c          maxes out at 700mb, low levels count a little less
       
        DO J=1,JM
        DO I=1,IM
         DO L=1,MAXLEV
         ENDDO
         SKY(I,J)=0.
         SKY1=0.
         SKY2=0.
         SKY3=0.
         SKY4=0.
         SKY5=0.
         SKY6=0.
         SKY7=0.
c  don't count 1st level fog
         DO L=2,MAXLEV
c  When level 1 has 50% coverage, then 50% coverage
c  at level 2 covers 50% of the remaining clear sky,
c  (so now 75%) and 50% coverage at level 3 covers
c  50% of the remaining clear sky (now 87%), etc.

          IF (CFR(I,J,L).GT.0.) THEN
           SKY1=CFR(I,J,L)
           DO M=L,MAXLEV
            IF(CFR(I,J,M).EQ.0.) GOTO 333
            IF(CFR(I,J,M).GT.SKY1) SKY1=CFR(I,J,M)
           ENDDO
          ENDIF
         ENDDO
 333     CONTINUE
         IF(L.GE.MAXLEV) GOTO 369
         IF(M.GE.MAXLEV) GOTO 369
         DO N=M+1,MAXLEV
           IF (CFR(I,J,N).GT.0.) THEN
            SKY2=CFR(I,J,N)
            DO NN=N+1,MAXLEV
             IF(CFR(I,J,NN).EQ.0.) GOTO 334
             IF(CFR(I,J,NN).GT.SKY2) SKY2=CFR(I,J,NN)
            ENDDO
           ENDIF
         ENDDO
 334      CONTINUE
         IF(N.GE.MAXLEV) GOTO 369
         IF(NN.GE.MAXLEV) GOTO 369
         DO P=NN+1,MAXLEV
           IF (CFR(I,J,P).GT.0.) THEN
            SKY3=CFR(I,J,P)
            DO PP=P+1,MAXLEV
             IF(CFR(I,J,PP).EQ.0.) GOTO 335
             IF(CFR(I,J,PP).GT.SKY3) SKY3=CFR(I,J,PP)
            ENDDO
           ENDIF
         ENDDO
 335     CONTINUE
         IF(P.GE.MAXLEV) GOTO 369
         IF(PP.GE.MAXLEV) GOTO 369
         DO R=PP+1,MAXLEV
           IF (CFR(I,J,R).GT.0.) THEN
            SKY4=CFR(I,J,R)
            DO RR=R+1,MAXLEV
             IF(CFR(I,J,RR).EQ.0.) GOTO 336
             IF(CFR(I,J,RR).GT.SKY4) SKY4=CFR(I,J,RR)
            ENDDO
           ENDIF
         ENDDO
 336     CONTINUE
         IF(R.GE.MAXLEV) GOTO 369
         IF(RR.GE.MAXLEV) GOTO 369
         DO S=RR+1,MAXLEV
           IF (CFR(I,J,S).GT.0.) THEN
            SKY5=CFR(I,J,S)
            DO SS=S+1,MAXLEV
             IF(CFR(I,J,SS).EQ.0.) GOTO 337
             IF(CFR(I,J,SS).GT.SKY5) SKY5=CFR(I,J,SS)
            ENDDO
           ENDIF
         ENDDO
 337     CONTINUE
         IF(S.GE.MAXLEV) GOTO 369
         IF(SS.GE.MAXLEV) GOTO 369
         DO W=SS+1,MAXLEV
           IF (CFR(I,J,W).GT.0.) THEN
            SKY6=CFR(I,J,W)
            DO WW=W+1,MAXLEV
             IF(CFR(I,J,WW).EQ.0.) GOTO 338
             IF(CFR(I,J,WW).GT.SKY6) SKY6=CFR(I,J,WW)
            ENDDO
           ENDIF
         ENDDO
 338     CONTINUE
         IF(W.GE.MAXLEV) GOTO 369
         IF(WW.GE.MAXLEV) GOTO 369
         DO X=WW+1,MAXLEV
           IF (CFR(I,J,X).GT.0.) THEN
            SKY7=CFR(I,J,X)
            DO XX=X+1,MAXLEV
             IF(CFR(I,J,XX).EQ.0.) GOTO 339
             IF(CFR(I,J,XX).GT.SKY7) SKY7=CFR(I,J,XX)
            ENDDO
           ENDIF
         ENDDO
 339     CONTINUE

 369     SKY1=SKY1/100.
         SKY2=SKY2/100.
         SKY3=SKY3/100.
         SKY4=SKY4/100.
         SKY5=SKY5/100.
         SKY6=SKY6/100.
         SKY7=SKY7/100.
         SKY(I,J)=(1.-(1.-SKY1)*(1.-SKY2)*(1.-SKY3)*(1.-SKY4)*
     x               (1.-SKY5)*(1.-SKY6)*(1.-SKY7))*100.
        ENDDO
        ENDDO       
c        call smoothpm(sky,2)
        CALL BOUND (SKY,0.,100.)

        ID(1:25) = 0
        ID(8)=71
        ID(9)=1
        DEC=3.0
        CALL GRIBIT(ID,RITEHD,SKY,DATE,FHR,70,DEC)

        ID(1:25) = 0
        ID(2)=129
        ID(8)=212
        ID(9)=200
        DEC=3.0
        CALL GRIBIT(ID,RITEHD,REFC,DATE,FHR,70,DEC)

c========================================================================
c calcSnowLevel - takes sounding of the wetbulb temperature and finds the
c   lowest elevation (above ground) where wetbulb crosses from
c   above freezing to below freezing. When top wetbulb is above
c   freezing - puts in height of top level.   We now use this
c   field straight out of the NAM. 
c

      ID(1:25) = 0
      ID(8)=7
      ID(9)=245
      DEC=3.0
      CALL GRIBIT(ID,RITEHD,WETFRZ,DATE,FHR,70,DEC)

C VISIBILITY

      ID(1:25) = 0
      ID(8)=20
      ID(9)=1
      DEC=2.7
      CALL GRIBIT(ID,RITEHD,VIS,DATE,FHR,70,DEC)

c==========================================================================
c  TransWind - the average winds in the layer between the surface
c              and the mixing height.
c--------------------------------------------------------------------------

      DO J=1,JM
      DO I=1,IM
        MGD=SQRT(DOWNU(I,J)*DOWNU(I,J)+DOWNV(I,J)*DOWNV(I,J)) 
        UTOT=0.
        VTOT=0.
        COUNT=0.
        LMBL=PBLMARK(I,J)
       DO L=1,LMBL
         UTOT=UTOT+UWND(I,J,L)
         VTOT=VTOT+VWND(I,J,L)
         COUNT=COUNT+1
       ENDDO
        UTRANS=UTOT/COUNT
        VTRANS=VTOT/COUNT
        MGTRANS(I,J)=SQRT(UTRANS*UTRANS+VTRANS*VTRANS) 
        IF (MGTRANS(I,J).EQ.0.) THEN
         DIRTRANS(I,J)=0.
        ELSE
        DIRTRANS(I,J)=ATAN2(-UTRANS,-VTRANS) / 0.0174
        ENDIF
        IF(DIRTRANS(I,J).LT.0.) DIRTRANS(I,J)=DIRTRANS(I,J)+360.0
        IF(DIRTRANS(I,J).GT.360.) DIRTRANS(I,J)=DIRTRANS(I,J)-360.0    
      ENDDO
      ENDDO

      ID(1:25) = 0
      ID(8)=31
      ID(9)=220
      DEC=3.0
      CALL GRIBIT(ID,RITEHD,DIRTRANS,DATE,FHR,70,DEC)

      ID(1:25) = 0
      ID(8)=32
      ID(9)=220
      DEC=-3.0
      CALL GRIBIT(ID,RITEHD,MGTRANS,DATE,FHR,70,DEC)

c  compute BL RH

      DO J=1,JM
      DO I=1,IM
       BLH=PBLMARK(I,J)
       SUM=0.
       LEVS=0.
       DO L=1, BLH
        SUM=SUM+RH(I,J,L)
        LEVS=LEVS+1
       ENDDO
        BLR(I,J)=(SUM/LEVS)*100.
      ENDDO
      ENDDO
      CALL BOUND(BLR,0.,100.)

      ID(1:25) = 0
      ID(8)=52
      ID(9)=220
      DEC=3.0
      CALL GRIBIT(ID,RITEHD,BLR,DATE,FHR,70,DEC)

c========================================================================
c  MixHgt - the height to which a parcel above a 'fire' would rise
c    (in height) above ground level (in feet).
c
c  Calculated by assuming a parcel above a fire is VERY hot - but the fire
c  is very small - so that entrainment quickly makes it only a few degrees
c  warmer than the environment.  Ideally would want to consider moisture
c  and entrainment - but this is a very simple first guess.

      DO J=1,JM
      DO I=1,IM
       firetheta=((P1000/PSFC(I,J))**CAPA)*(T2(I,J)+2.0)
       DO L=2,MAXLEV
         theta=((P1000/PMID(I,J,L))**CAPA)*(T(I,J,L))
         IF (theta.gt.firetheta) THEN
          MIXHGT(I,J)=HGHT(I,J,L)-ZSFC(I,J)
          GOTO 321
         ENDIF
       ENDDO
 321   CONTINUE
      ENDDO
      ENDDO

      ID(1:25) = 0
      ID(8)=8
      ID(9)=220
      DEC=-3.0
      CALL GRIBIT(ID,RITEHD,MIXHGT,DATE,FHR,70,DEC)

c--------------------------------------------------------------------------
c LAL - Based mainly on lifted index.  Adds more when RH at top of BL is
c       high, but RH at bottom of BL is low.
c--------------------------------------------------------------------------

      DO J=1,JM
      DO I=1,IM
       IF (BLI(I,J).LT.-5.) THEN
         LLAL=4.
       ELSE IF (BLI(I,J).LT.-3) THEN
         LLAL=3.
       ELSE IF (BLI(I,J).LT.0) THEN
         LLAL=2.
       ELSE
         LLAL=1.
       ENDIF

c   Add more when RH at top of BL is greater than
c      than 70% and RH at bottom of BL is less than 30

       RH1TOT=0.
       RH1SUM=0.
       RH2TOT=0.
       RH2SUM=0.
       DO L=1,40
        IF(PSFC(I,J)-PMID(I,J,L).LT.3000.) THEN
          RH1TOT=RH1TOT+RH(I,J,L)
          RH1SUM=RH1SUM+1.
        ENDIF
        IF(PSFC(I,J)-PMID(I,J,L).LT.18000. .AND.
     x     PSFC(I,J)-PMID(I,J,L).GT.15000.) THEN
          RH2TOT=RH2TOT+RH(I,J,L)
          RH2SUM=RH2SUM+1.
        ENDIF
       ENDDO
       RH1=RH1TOT/RH1SUM
       RH2=RH2TOT/RH2SUM 
       IF (RH2.GT.0.8 .AND. RH1.LT.0.2) THEN
        LAL(I,J)=LLAL+1.
       ELSE
        LAL(I,J)=LLAL
       ENDIF
       IF (LAL(I,J).LT.-18.) THEN
        LAL(I,J)=1.
       ENDIF
      ENDDO
      ENDDO
      ID(1:25) = 0
      ID(8)=132
      ID(9)=1
      DEC=2.0
      CALL GRIBIT(ID,RITEHD,LAL,DATE,FHR,70,DEC)

c=======================================================================
c
c  Calculate Haines Index
c  type is "LOW", "MEDIUM", "HIGH"
c  NOTE, the default haines index calcaulation is defined by:
c  self.whichHainesIndex, which can be set to "LOW", "MEDIUM", "HIGH".
c
c=======================================================================

c      DO J=1,JM
c      DO I=1,IM
c       IF(PSFC(I,J).GT.95000.) THEN
c        HAT=T950(I,J)-T850(I,J)
c        TMOIS=T850(I,J)-273.15
c        RHMOIS=RH850(I,J)
c        ST1=8
c        ST2=3
c        MT1=10
c        MT2=5
c       ELSE IF(PSFC(I,J).GT.85000.) THEN
c        HAT=T850(I,J)-T700(I,J)
c        TMOIS=T850(I,J)-273.15
c        RHMOIS=RH850(I,J)
c        ST1=11
c        ST2=5
c        MT1=13
c        MT2=5
c       ELSE
c        HAT=T700(I,J)-T500(I,J)
c        TMOIS=T700(I,J)-273.15
c        RHMOIS=RH700(I,J)
c        ST1=22
c        ST2=17
c        MT1=21
c        MT2=14
c       ENDIF
c       TERM=log10(RHMOIS) / 7.5 + (TMOIS / (TMOIS + 237.3))
c       DPMOIS=(TERM * 237.3) / (1.0 - TERM)
c       HAINESM=TMOIS-DPMOIS 
c       SLOPET=1/(ST1-ST2)
c       INTT=1.5-(ST2-0.5)*SLOPET
c       HAINEST=(SLOPET*HAT)+INTT
c       SLOPEM=1/(MT1-MT2)
c       INTM=1.5-(MT2-0.5)*SLOPEM
c       HAINESM=(SLOPEM*DPMOIS)+INTM
c       HAINES(I,J)=HAINEST+HAINESM
c      ENDDO
c      ENDDO
c      ID(1:25) = 0
c      ID(2)=130
c      ID(8)=241
c      ID(9)=1
c      DEC=3.0
c      CALL GRIBIT(ID,RITEHD,HAINES,DATE,FHR,70,DEC)
      ENDIF 

      IF (CYC .EQ. 00 .OR. CYC .EQ. 12) THEN
       IF (MOD(FHR,12).NE.0) THEN
        print *, 'going to write minmax ', fhr
        RITEHD = .TRUE.
        ID(1:25) = 0
        ID(8)=11
        ID(9)=1
        DEC=-2.0
        CALL GRIBIT(ID,RITEHD,DOWNT,DATE,FHR,71,DEC)
 
        ID(1:25) = 0
        ID(8)=17
        ID(9)=1
        DEC=-2.0
        CALL GRIBIT(ID,RITEHD,DOWNDEW,DATE,FHR,71,DEC)
       ENDIF
      ELSE
       IF (MOD(FHR-6,12).NE.0) THEN
        print *, 'going to write minmax ', fhr
        RITEHD = .TRUE.
        ID(1:25) = 0
        ID(8)=11
        ID(9)=1
        DEC=-2.0
        CALL GRIBIT(ID,RITEHD,DOWNT,DATE,FHR,71,DEC)

        ID(1:25) = 0
        ID(8)=17
        ID(9)=1
        DEC=-2.0
        CALL GRIBIT(ID,RITEHD,DOWNDEW,DATE,FHR,71,DEC)
       ENDIF
      ENDIF

c  write older T/Td data for max/min to grib file
      IF (CYC .EQ. 00 .OR. CYC .EQ. 12) THEN
        IF (MOD(FHR,3).EQ.0 .AND. MOD(FHR,12).NE.0
     x     .AND. FHR .NE. 0) THEN
       DO J=1,JM
       DO I=1,IM
         TH1(I,J)=THOLD(I,J,2)
         TH2(I,J)=THOLD(I,J,3)
         DH1(I,J)=DHOLD(I,J,2)
         DH2(I,J)=DHOLD(I,J,3)
       ENDDO
       ENDDO

       HOUR=FHR-1
       ID(1:25) = 0
       ID(8)=11
       ID(9)=1
       DEC=-2.0
       CALL GRIBIT(ID,RITEHD,TH1,DATE,HOUR,70,DEC)

       HOUR=FHR-2
       ID(1:25) = 0
       ID(8)=11
       ID(9)=1
       DEC=-2.0
       CALL GRIBIT(ID,RITEHD,TH2,DATE,HOUR,70,DEC)

       HOUR=FHR-1
       ID(1:25) = 0
       ID(8)=17
       ID(9)=1
       DEC=-2.0
       CALL GRIBIT(ID,RITEHD,DH1,DATE,HOUR,70,DEC)

       HOUR=FHR-2
       ID(1:25) = 0
       ID(8)=17
       ID(9)=1
       DEC=-2.0
       CALL GRIBIT(ID,RITEHD,DH2,DATE,HOUR,70,DEC)
      ENDIF
      ELSE
       IF (MOD(FHR,3).EQ.0 .AND. MOD(FHR+6,12).NE.0
     x     .AND. FHR .NE. 0) THEN
        print *, 'writing older data'
        DO J=1,JM
        DO I=1,IM
          TH1(I,J)=THOLD(I,J,2)
          TH2(I,J)=THOLD(I,J,3)
          DH1(I,J)=DHOLD(I,J,2)
          DH2(I,J)=DHOLD(I,J,3)
        ENDDO
        ENDDO

        HOUR=FHR-1
        ID(1:25) = 0
        ID(8)=11
        ID(9)=1
        DEC=-2.0
        CALL GRIBIT(ID,RITEHD,TH1,DATE,HOUR,70,DEC)

        HOUR=FHR-2
        ID(1:25) = 0
        ID(8)=11
        ID(9)=1
        DEC=-2.0
        CALL GRIBIT(ID,RITEHD,TH2,DATE,HOUR,70,DEC)

        HOUR=FHR-1
        ID(1:25) = 0
        ID(8)=17
        ID(9)=1
        DEC=-2.0
        CALL GRIBIT(ID,RITEHD,DH1,DATE,HOUR,70,DEC)

        HOUR=FHR-2
        ID(1:25) = 0
        ID(8)=17
        ID(9)=1
        DEC=-2.0
        CALL GRIBIT(ID,RITEHD,DH2,DATE,HOUR,70,DEC) 
       ENDIF
      ENDIF

c  compute max/min temps for 3,6,9,12.....
      IF (MOD(FHR,3).EQ.0 .AND. FHR .NE. 0) THEN
       print *, 'computing maxmin3'
       DO J=1,JM
       DO I=1,IM
        TMAX3(I,J)=-9999.
        RHMAX3(I,J)=-9999.
        TMIN3(I,J)=9999.
        RHMIN3(I,J)=9999.
        IF (MOD(FHR,12).EQ.0) THEN
         TMPT=THOLD(I,J,1)
         TMPD=DHOLD(I,J,1)
        ELSE
         TMPT=DOWNT(I,J)
         TMPD=DOWNDEW(I,J) 
        ENDIF
        THOLD(I,J,1)=DOWNT(I,J)
        DHOLD(I,J,1)=DOWNDEW(I,J)
        DO L=1,3
         IF(THOLD(I,J,L).GT.TMAX3(I,J)) TMAX3(I,J)=THOLD(I,J,L)
         IF(THOLD(I,J,L).LT.TMIN3(I,J)) TMIN3(I,J)=THOLD(I,J,L)
          QX=PQ0/PSFC(I,J)*EXP(A2*(DHOLD(I,J,L)-A3)/
     x        (DHOLD(I,J,L)-A4))
          QSX=PQ0/PSFC(I,J)*EXP(A2*(THOLD(I,J,L)-A3)/
     x        (THOLD(I,J,L)-A4))
          RELH=100*QX/QSX
          IF(RELH.GT.RHMAX3(I,J)) RHMAX3(I,J)=RELH
          IF(RELH.LT.RHMIN3(I,J)) RHMIN3(I,J)=RELH
c switch back the thold and dhold values since we need 
c   the originals for the 12-hr values
        ENDDO
          THOLD(I,J,1)=TMPT
          DHOLD(I,J,1)=TMPD
       ENDDO
       ENDDO

       CALL BOUND(RHMAX3,0.,100.)
       CALL BOUND(RHMIN3,0.,100.)
 
       ID(1:25) = 0
       ID(8)=15
       ID(9)=1
       ID(18)=FHR3
       ID(19)=FHR
       ID(20)=4
       DEC=-2.0
       CALL GRIBIT(ID,RITEHD,TMAX3,DATE,FHR,70,DEC)

       ID(1:25) = 0
       ID(8)=16
       ID(9)=1
       ID(18)=FHR3
       ID(19)=FHR
       ID(20)=4
       DEC=-2.0
       CALL GRIBIT(ID,RITEHD,TMIN3,DATE,FHR,70,DEC)

       ID(1:25) = 0
       ID(2)=129
       ID(8)=218
       ID(9)=1
       ID(18)=FHR3
       ID(19)=FHR
       ID(20)=4
       DEC=3.0
       CALL GRIBIT(ID,RITEHD,RHMAX3,DATE,FHR,70,DEC)

       ID(1:25) = 0
       ID(2)=129
       ID(8)=217
       ID(9)=1
       ID(18)=FHR3
       ID(19)=FHR
       ID(20)=4
       DEC=3.0
       CALL GRIBIT(ID,RITEHD,RHMIN3,DATE,FHR,70,DEC)
      ENDIF

c  now compute the max and min values if end of 12-hr period
      IF (CYC .EQ. 00 .OR. CYC .EQ. 12) THEN
       IF (MOD(FHR,12).EQ.0 .AND. FHR .NE. 0) THEN 
        print *, '12-hr max min'
        DO J=1,JM
        DO I=1,IM
         TMAX12(I,J)=-9999.
         TMIN12(I,J)=9999.
         RHMAX12(I,J)=-9999.
         RHMIN12(I,J)=9999.
         THOLD(I,J,1)=DOWNT(I,J)
         DHOLD(I,J,1)=DOWNDEW(I,J)
         DO L=1,12
          IF(THOLD(I,J,L).GT.TMAX12(I,J)) TMAX12(I,J)=THOLD(I,J,L)
          IF(THOLD(I,J,L).LT.TMIN12(I,J)) TMIN12(I,J)=THOLD(I,J,L)
          QX=PQ0/PSFC(I,J)*EXP(A2*(DHOLD(I,J,L)-A3)/
     x        (DHOLD(I,J,L)-A4))
          QSX=PQ0/PSFC(I,J)*EXP(A2*(THOLD(I,J,L)-A3)/
     x        (THOLD(I,J,L)-A4))
          RELH=100*QX/QSX
          IF(RELH.GT.RHMAX12(I,J)) RHMAX12(I,J)=RELH
          IF(RELH.LT.RHMIN12(I,J)) RHMIN12(I,J)=RELH
         ENDDO
         ENDDO
         ENDDO
         CALL BOUND(RHMAX12,0.,100.)
         CALL BOUND(RHMIN12,0.,100.)

         ID(1:25) = 0
         ID(8)=15
         ID(9)=1
         ID(18)=FHR12
         ID(19)=FHR
         ID(20)=4
         DEC=-2.0
         CALL GRIBIT(ID,RITEHD,TMAX12,DATE,FHR,70,DEC)
         ID(1:25) = 0
         ID(8)=16
         ID(9)=1
         ID(18)=FHR12
         ID(19)=FHR
         ID(20)=4
         DEC=-2.0
         CALL GRIBIT(ID,RITEHD,TMIN12,DATE,FHR,70,DEC)

         ID(1:25) = 0
         ID(2)=129
         ID(8)=218
         ID(9)=1
         ID(18)=FHR12
         ID(19)=FHR
         ID(20)=4
         DEC=3.0
         CALL GRIBIT(ID,RITEHD,RHMAX12,DATE,FHR,70,DEC)

         ID(1:25) = 0
         ID(2)=129
         ID(8)=217
         ID(9)=1
         ID(18)=FHR12
         ID(19)=FHR
         ID(20)=4
         DEC=3.0
         CALL GRIBIT(ID,RITEHD,RHMIN12,DATE,FHR,70,DEC)

        ENDIF
c  6/18 cycle
       ELSE
        IF (FHR .EQ. 18 .OR. FHR .EQ. 30 .OR. FHR .EQ. 42 
     x   .OR. FHR .EQ. 54 .OR. FHR .EQ. 66 .OR. 
     x    FHR .EQ. 78) THEN
        DO J=1,JM
        DO I=1,IM
         TMAX12(I,J)=-9999.
         RHMAX12(I,J)=-9999.
         TMIN12(I,J)=9999.
         RHMIN12(I,J)=9999.
         THOLD(I,J,1)=DOWNT(I,J)
         DHOLD(I,J,1)=DOWNDEW(I,J)
         DO L=1,12
          IF(THOLD(I,J,L).GT.TMAX12(I,J)) TMAX12(I,J)=THOLD(I,J,L)
          IF(THOLD(I,J,L).LT.TMIN12(I,J)) TMIN12(I,J)=THOLD(I,J,L)
          QX=PQ0/PSFC(I,J)*EXP(A2*(DHOLD(I,J,L)-A3)/
     x        (DHOLD(I,J,L)-A4))
          QSX=PQ0/PSFC(I,J)*EXP(A2*(THOLD(I,J,L)-A3)/
     x        (THOLD(I,J,L)-A4))
          RELH=100*QX/QSX
          IF(RELH.GT.RHMAX12(I,J)) RHMAX12(I,J)=RELH
          IF(RELH.LT.RHMIN12(I,J)) RHMIN12(I,J)=RELH
         ENDDO
        ENDDO
        ENDDO
        CALL BOUND(RHMAX12,0.,100.)
        CALL BOUND(RHMIN12,0.,100.)

        ID(1:25) = 0
        ID(8)=15
        ID(9)=1
        ID(18)=FHR12
        ID(19)=FHR
        ID(20)=4
        DEC=-2.0
        CALL GRIBIT(ID,RITEHD,TMAX12,DATE,FHR,70,DEC)

        ID(1:25) = 0
        ID(8)=16
        ID(9)=1
        ID(18)=FHR12
        ID(19)=FHR
        ID(20)=4
        DEC=-2.0
        CALL GRIBIT(ID,RITEHD,TMIN12,DATE,FHR,70,DEC)

        ID(1:25) = 0
        ID(2)=129
        ID(8)=218
        ID(9)=1
        ID(18)=FHR12
        ID(19)=FHR
        ID(20)=4
        DEC=3.0
        CALL GRIBIT(ID,RITEHD,RHMAX12,DATE,FHR,70,DEC)

        ID(1:25) = 0
        ID(2)=129
        ID(8)=217
        ID(9)=1
        ID(18)=FHR12
        ID(19)=FHR
        ID(20)=4
        DEC=3.0
        CALL GRIBIT(ID,RITEHD,RHMIN12,DATE,FHR,70,DEC)
       ENDIF
       ENDIF
       print *, 'completed main'
      STOP
      END

