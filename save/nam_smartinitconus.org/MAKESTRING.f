      SUBROUTINE MAKESTRING(IRAIN,ISNOW,IZR,IIP,LIFT,PROB,STRING,
     X       GRIDWX) 

      PARAMETER(IM=1073,JM=689,MAXLEV=60)
      INTEGER IRAIN(IM,JM),ISNOW(IM,JM),IZR(IM,JM),IIP(IM,JM)
      INTEGER START
      REAL WX(IM,JM),LIFT(IM,JM),PROB(IM,JM),THUNDER(IM,JM)
      REAL GRIDWX(IM,JM)
      CHARACTER *50 STRING(IM,JM),TEMP,TRACK(200)

c  weather type
c  1-rain 2-snow 3-frzrain 4-sleet 5-rw 6-sw 7-frzrainshower
c    8-sleetshower     we won't use mixes
                                                                                     
        DO J=1,JM
        DO I=1,IM
         WX(I,J)=0.
         IF (IRAIN(I,J).EQ. 1) WX(I,J)=1.
         IF (ISNOW(I,J).EQ. 1) WX(I,J)=2.
         IF (IZR(I,J).EQ. 1) WX(I,J)=3.
         IF (IIP(I,J).EQ. 1) WX(I,J)=4.
         IF (WX(I,J).GT.0.) THEN
           IF (LIFT(I,J).LT. 2.) WX(I,J)=WX(I,J)+4.
         ENDIF

c      thunder is totally separate from PoP, only related to
c       the instability. SChc  for LI <-1, Chc for LI<-3,
c       Lkly for LI<-5, Def for LI<-8
                                                                                     
         IF(LIFT(I,J).LE.-8.) THEN
          THUNDER(I,J)=4.
         ELSE IF (LIFT(I,J).LE.-5.) THEN
          THUNDER(I,J)=3.
         ELSE IF (LIFT(I,J).LE.-3) THEN
          THUNDER(I,J)=2.
         ELSE IF (LIFT(I,J).LE.-1) THEN
          THUNDER(I,J)=1.
         ELSE
          THUNDER(I,J)=0.
         ENDIF
        ENDDO
        ENDDO
      
        DO J=1,JM
        DO I=1,IM
         IF (WX(I,J).EQ. 0.) THEN
           STRING(I,J)="<NoCov>:<NoWx>:<NoInten>:<NoVis>:"
           GRIDWX(I,J)=0.
         ELSE IF (WX(I,J).EQ.1) THEN
          IF (PROB(I,J).LT.25.) THEN
           IF (THUNDER(I,J).EQ.0.) THEN
            STRING(I,J)="SChc:R:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.1.) THEN
            STRING(I,J)="SChc:R:-:<NoVis>^SChc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.2.) THEN
            STRING(I,J)="SChc:R:-:<NoVis>^Chc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.3.) THEN
            STRING(I,J)="SChc:R:-:<NoVis>^Lkly:T:-:<NoVis>:"
           ELSE
            STRING(I,J)="SChc:R:-:<NoVis>^Def:T:-:<NoVis>:"
           ENDIF
          ELSE IF (PROB(I,J).LT.55.) THEN
           IF (THUNDER(I,J).EQ.0.) THEN
            STRING(I,J)="Chc:R:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.1.) THEN
            STRING(I,J)="Chc:R:-:<NoVis>^SChc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.2.) THEN
            STRING(I,J)="Chc:R:-:<NoVis>^Chc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.3.) THEN
            STRING(I,J)="Chc:R:-:<NoVis>^Lkly:T:-:<NoVis>:"
           ELSE
            STRING(I,J)="Chc:R:-:<NoVis>^Def:T:-:<NoVis>:"
           ENDIF
          ELSE IF (PROB(I,J).LT.75.) THEN
           IF (THUNDER(I,J).EQ.0.) THEN
            STRING(I,J)="Lkly:R:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.1.) THEN
            STRING(I,J)="Lkly:R:-:<NoVis>^SChc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.2.) THEN
            STRING(I,J)="Lkly:R:-:<NoVis>^Chc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.3.) THEN
            STRING(I,J)="Lkly:R:-:<NoVis>^Lkly:T:-:<NoVis>:"
           ELSE
            STRING(I,J)="Lkly:R:-:<NoVis>^Def:T:-:<NoVis>:"
           ENDIF
          ELSE
           IF (THUNDER(I,J).EQ.0.) THEN
            STRING(I,J)="Def:R:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.1.) THEN
            STRING(I,J)="Def:R:-:<NoVis>^SChc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.2.) THEN
            STRING(I,J)="Def:R:-:<NoVis>^Chc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.3.) THEN
            STRING(I,J)="Def:R:-:<NoVis>^Lkly:T:-:<NoVis>:"
           ELSE
            STRING(I,J)="Def:R:-:<NoVis>^Def:T:-:<NoVis>:"
           ENDIF
          ENDIF
         ELSE IF (WX(I,J).EQ.2) THEN
          IF (PROB(I,J).LT.25.) THEN
           IF (THUNDER(I,J).EQ.0.) THEN
            STRING(I,J)="SChc:S:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.1.) THEN
            STRING(I,J)="SChc:S:-:<NoVis>^SChc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.2.) THEN
            STRING(I,J)="SChc:S:-:<NoVis>^Chc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.3.) THEN
            STRING(I,J)="SChc:S:-:<NoVis>^Lkly:T:-:<NoVis>:"
           ELSE
            STRING(I,J)="SChc:S:-:<NoVis>^Def:T:-:<NoVis>:"
           ENDIF
          ELSE IF (PROB(I,J).LT.55.) THEN
           IF (THUNDER(I,J).EQ.0.) THEN
            STRING(I,J)="Chc:S:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.1.) THEN
            STRING(I,J)="Chc:S:-:<NoVis>^SChc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.2.) THEN
            STRING(I,J)="Chc:S:-:<NoVis>^Chc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.3.) THEN
            STRING(I,J)="Chc:S:-:<NoVis>^Lkly:T:-:<NoVis>:"
           ELSE
            STRING(I,J)="Chc:S:-:<NoVis>^Def:T:-:<NoVis>:"
           ENDIF
          ELSE IF (PROB(I,J).LT.75.) THEN
           IF (THUNDER(I,J).EQ.0.) THEN
            STRING(I,J)="Lkly:S:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.1.) THEN
            STRING(I,J)="Lkly:S:-:<NoVis>^SChc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.2.) THEN
            STRING(I,J)="Lkly:S:-:<NoVis>^Chc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.3.) THEN
            STRING(I,J)="Lkly:S:-:<NoVis>^Lkly:T:-:<NoVis>:"
           ELSE
            STRING(I,J)="Lkly:S:-:<NoVis>^Def:T:-:<NoVis>:"
           ENDIF
          ELSE
           IF (THUNDER(I,J).EQ.0.) THEN
            STRING(I,J)="Def:S:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.1.) THEN
            STRING(I,J)="Def:S:-:<NoVis>^SChc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.2.) THEN
            STRING(I,J)="Def:S:-:<NoVis>^Chc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.3.) THEN
            STRING(I,J)="Def:S:-:<NoVis>^Lkly:T:-:<NoVis>:"
           ELSE
            STRING(I,J)="Def:S:-:<NoVis>^Def:T:-:<NoVis>:"
           ENDIF 
         ENDIF
        ELSE IF (WX(I,J).EQ.3) THEN
          IF (PROB(I,J).LT.25.) THEN
           IF (THUNDER(I,J).EQ.0.) THEN
            STRING(I,J)="SChc:ZR:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.1.) THEN
            STRING(I,J)="SChc:ZR:-:<NoVis>^SChc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.2.) THEN
            STRING(I,J)="SChc:ZR:-:<NoVis>^Chc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.3.) THEN
            STRING(I,J)="SChc:ZR:-:<NoVis>^Lkly:T:-:<NoVis>:"
           ELSE
            STRING(I,J)="SChc:ZR:-:<NoVis>^Def:T:-:<NoVis>:"
           ENDIF
          ELSE IF (PROB(I,J).LT.55.) THEN
           IF (THUNDER(I,J).EQ.0.) THEN
            STRING(I,J)="Chc:ZR:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.1.) THEN
            STRING(I,J)="Chc:ZR:-:<NoVis>^SChc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.2.) THEN
            STRING(I,J)="Chc:ZR:-:<NoVis>^Chc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.3.) THEN
            STRING(I,J)="Chc:ZR:-:<NoVis>^Lkly:T:-:<NoVis>:"
           ELSE
            STRING(I,J)="Chc:ZR:-:<NoVis>^Def:T:-:<NoVis>:"
           ENDIF
          ELSE IF (PROB(I,J).LT.75.) THEN
           IF (THUNDER(I,J).EQ.0.) THEN
            STRING(I,J)="Lkly:ZR:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.1.) THEN
            STRING(I,J)="Lkly:ZR:-:<NoVis>^SChc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.2.) THEN
            STRING(I,J)="Lkly:ZR:-:<NoVis>^Chc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.3.) THEN
            STRING(I,J)="Lkly:ZR:-:<NoVis>^Lkly:T:-:<NoVis>:"
           ELSE
            STRING(I,J)="Lkly:ZR:-:<NoVis>^Def:T:-:<NoVis>:"
           ENDIF
          ELSE
           IF (THUNDER(I,J).EQ.0.) THEN
            STRING(I,J)="Def:ZR:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.1.) THEN
            STRING(I,J)="Def:ZR:-:<NoVis>^SChc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.2.) THEN
            STRING(I,J)="Def:ZR:-:<NoVis>^Chc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.3.) THEN
            STRING(I,J)="Def:ZR:-:<NoVis>^Lkly:T:-:<NoVis>:"
           ELSE
            STRING(I,J)="Def:ZR:-:<NoVis>^Def:T:-:<NoVis>:"
           ENDIF
         ENDIF
        ELSE IF (WX(I,J).EQ.4) THEN
          IF (PROB(I,J).LT.25.) THEN
           IF (THUNDER(I,J).EQ.0.) THEN
            STRING(I,J)="SChc:IP:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.1.) THEN
            STRING(I,J)="SChc:IP:-:<NoVis>^SChc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.2.) THEN
            STRING(I,J)="SChc:IP:-:<NoVis>^Chc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.3.) THEN
            STRING(I,J)="SChc:IP:-:<NoVis>^Lkly:T:-:<NoVis>:"
           ELSE
            STRING(I,J)="SChc:IP:-:<NoVis>^Def:T:-:<NoVis>:"
           ENDIF
          ELSE IF (PROB(I,J).LT.55.) THEN
           IF (THUNDER(I,J).EQ.0.) THEN
            STRING(I,J)="Chc:IP:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.1.) THEN
            STRING(I,J)="Chc:IP:-:<NoVis>^SChc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.2.) THEN
            STRING(I,J)="Chc:IP:-:<NoVis>^Chc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.3.) THEN
            STRING(I,J)="Chc:IP:-:<NoVis>^Lkly:T:-:<NoVis>:"
           ELSE
            STRING(I,J)="Chc:IP:-:<NoVis>^Def:T:-:<NoVis>:"
           ENDIF
          ELSE IF (PROB(I,J).LT.75.) THEN
           IF (THUNDER(I,J).EQ.0.) THEN
            STRING(I,J)="Lkly:IP:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.1.) THEN
            STRING(I,J)="Lkly:IP:-:<NoVis>^SChc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.2.) THEN
            STRING(I,J)="Lkly:IP:-:<NoVis>^Chc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.3.) THEN
            STRING(I,J)="Lkly:IP:-:<NoVis>^Lkly:T:-:<NoVis>:"
           ELSE
            STRING(I,J)="Lkly:IP:-:<NoVis>^Def:T:-:<NoVis>:"
           ENDIF
          ELSE
           IF (THUNDER(I,J).EQ.0.) THEN
            STRING(I,J)="Def:IP:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.1.) THEN
            STRING(I,J)="Def:IP:-:<NoVis>^SChc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.2.) THEN
            STRING(I,J)="Def:IP:-:<NoVis>^Chc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.3.) THEN
            STRING(I,J)="Def:IP:-:<NoVis>^Lkly:T:-:<NoVis>:"
           ELSE
            STRING(I,J)="Def:IP:-:<NoVis>^Def:T:-:<NoVis>:"
           ENDIF
         ENDIF
        ELSE IF (WX(I,J).EQ.5) THEN
          IF (PROB(I,J).LT.25.) THEN
           IF (THUNDER(I,J).EQ.0.) THEN
            STRING(I,J)="SChc:RW:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.1.) THEN
            STRING(I,J)="SChc:RW:-:<NoVis>^SChc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.2.) THEN
            STRING(I,J)="SChc:RW:-:<NoVis>^Chc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.3.) THEN
            STRING(I,J)="SChc:RW:-:<NoVis>^Lkly:T:-:<NoVis>:"
           ELSE
            STRING(I,J)="SChc:RW:-:<NoVis>^Def:T:-:<NoVis>:"
           ENDIF
          ELSE IF (PROB(I,J).LT.55.) THEN
           IF (THUNDER(I,J).EQ.0.) THEN
            STRING(I,J)="Chc:RW:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.1.) THEN
            STRING(I,J)="Chc:RW:-:<NoVis>^SChc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.2.) THEN
            STRING(I,J)="Chc:RW:-:<NoVis>^Chc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.3.) THEN
            STRING(I,J)="Chc:RW:-:<NoVis>^Lkly:T:-:<NoVis>:"
           ELSE
            STRING(I,J)="Chc:RW:-:<NoVis>^Def:T:-:<NoVis>:"
           ENDIF
          ELSE IF (PROB(I,J).LT.75.) THEN
           IF (THUNDER(I,J).EQ.0.) THEN
            STRING(I,J)="Lkly:RW:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.1.) THEN
            STRING(I,J)="Lkly:RW:-:<NoVis>^SChc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.2.) THEN
            STRING(I,J)="Lkly:RW:-:<NoVis>^Chc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.3.) THEN
            STRING(I,J)="Lkly:RW:-:<NoVis>^Lkly:T:-:<NoVis>:"
           ELSE
            STRING(I,J)="Lkly:RW:-:<NoVis>^Def:T:-:<NoVis>:"
           ENDIF
          ELSE
           IF (THUNDER(I,J).EQ.0.) THEN
            STRING(I,J)="Def:RW:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.1.) THEN
            STRING(I,J)="Def:RW:-:<NoVis>^SChc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.2.) THEN
            STRING(I,J)="Def:RW:-:<NoVis>^Chc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.3.) THEN
            STRING(I,J)="Def:RW:-:<NoVis>^Lkly:T:-:<NoVis>:"
           ELSE
            STRING(I,J)="Def:RW:-:<NoVis>^Def:T:-:<NoVis>:"
           ENDIF
         ENDIF
        ELSE IF (WX(I,J).EQ.6) THEN
          IF (PROB(I,J).LT.25.) THEN
           IF (THUNDER(I,J).EQ.0.) THEN
            STRING(I,J)="SChc:SW:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.1.) THEN
            STRING(I,J)="SChc:SW:-:<NoVis>^SChc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.2.) THEN
            STRING(I,J)="SChc:SW:-:<NoVis>^Chc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.3.) THEN
            STRING(I,J)="SChc:SW:-:<NoVis>^Lkly:T:-:<NoVis>:"
           ELSE
            STRING(I,J)="SChc:SW:-:<NoVis>^Def:T:-:<NoVis>:"
           ENDIF
          ELSE IF (PROB(I,J).LT.55.) THEN
           IF (THUNDER(I,J).EQ.0.) THEN
            STRING(I,J)="Chc:SW:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.1.) THEN
            STRING(I,J)="Chc:SW:-:<NoVis>^SChc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.2.) THEN
            STRING(I,J)="Chc:SW:-:<NoVis>^Chc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.3.) THEN
            STRING(I,J)="Chc:SW:-:<NoVis>^Lkly:T:-:<NoVis>:"
           ELSE
            STRING(I,J)="Chc:SW:-:<NoVis>^Def:T:-:<NoVis>:"
           ENDIF
          ELSE IF (PROB(I,J).LT.75.) THEN
           IF (THUNDER(I,J).EQ.0.) THEN
            STRING(I,J)="Lkly:SW:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.1.) THEN
            STRING(I,J)="Lkly:SW:-:<NoVis>^SChc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.2.) THEN
            STRING(I,J)="Lkly:SW:-:<NoVis>^Chc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.3.) THEN
            STRING(I,J)="Lkly:SW:-:<NoVis>^Lkly:T:-:<NoVis>:"
           ELSE
            STRING(I,J)="Lkly:SW:-:<NoVis>^Def:T:-:<NoVis>:"
           ENDIF
          ELSE
           IF (THUNDER(I,J).EQ.0.) THEN
            STRING(I,J)="Def:SW:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.1.) THEN
            STRING(I,J)="Def:SW:-:<NoVis>^SChc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.2.) THEN
            STRING(I,J)="Def:SW:-:<NoVis>^Chc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.3.) THEN
            STRING(I,J)="Def:SW:-:<NoVis>^Lkly:T:-:<NoVis>:"
           ELSE
            STRING(I,J)="Def:SW:-:<NoVis>^Def:T:-:<NoVis>:"
           ENDIF
         ENDIF
        ELSE IF (WX(I,J).EQ.7) THEN
          IF (PROB(I,J).LT.25.) THEN
           IF (THUNDER(I,J).EQ.0.) THEN
            STRING(I,J)="SChc:ZRW:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.1.) THEN
            STRING(I,J)="SChc:ZRW:-:<NoVis>^SChc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.2.) THEN
            STRING(I,J)="SChc:ZRW:-:<NoVis>^Chc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.3.) THEN
            STRING(I,J)="SChc:ZRW:-:<NoVis>^Lkly:T:-:<NoVis>:"
           ELSE
            STRING(I,J)="SChc:ZRW:-:<NoVis>^Def:T:-:<NoVis>:"
           ENDIF
          ELSE IF (PROB(I,J).LT.55.) THEN
           IF (THUNDER(I,J).EQ.0.) THEN
            STRING(I,J)="Chc:ZRW:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.1.) THEN
            STRING(I,J)="Chc:ZRW:-:<NoVis>^SChc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.2.) THEN
            STRING(I,J)="Chc:ZRW:-:<NoVis>^Chc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.3.) THEN
            STRING(I,J)="Chc:ZRW:-:<NoVis>^Lkly:T:-:<NoVis>:"
           ELSE
            STRING(I,J)="Chc:ZRW:-:<NoVis>^Def:T:-:<NoVis>:"
           ENDIF
          ELSE IF (PROB(I,J).LT.75.) THEN
           IF (THUNDER(I,J).EQ.0.) THEN
            STRING(I,J)="Lkly:ZRW:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.1.) THEN
            STRING(I,J)="Lkly:ZRW:-:<NoVis>^SChc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.2.) THEN
            STRING(I,J)="Lkly:ZRW:-:<NoVis>^Chc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.3.) THEN
            STRING(I,J)="Lkly:ZRW:-:<NoVis>^Lkly:T:-:<NoVis>:"
           ELSE
            STRING(I,J)="Lkly:ZRW:-:<NoVis>^Def:T:-:<NoVis>:"
           ENDIF
          ELSE
           IF (THUNDER(I,J).EQ.0.) THEN
            STRING(I,J)="Def:ZRW:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.1.) THEN
            STRING(I,J)="Def:ZRW:-:<NoVis>^SChc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.2.) THEN
            STRING(I,J)="Def:ZRW:-:<NoVis>^Chc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.3.) THEN
            STRING(I,J)="Def:ZRW:-:<NoVis>^Lkly:T:-:<NoVis>:"
           ELSE
            STRING(I,J)="Def:ZRW:-:<NoVis>^Def:T:-:<NoVis>:"
           ENDIF
         ENDIF
        ELSE IF (WX(I,J).EQ.8) THEN
          IF (PROB(I,J).LT.25.) THEN
           IF (THUNDER(I,J).EQ.0.) THEN
            STRING(I,J)="SChc:IPW:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.1.) THEN
            STRING(I,J)="SChc:IPW:-:<NoVis>^SChc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.2.) THEN
            STRING(I,J)="SChc:IPW:-:<NoVis>^Chc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.3.) THEN
            STRING(I,J)="SChc:IPW:-:<NoVis>^Lkly:T:-:<NoVis>:"
           ELSE
            STRING(I,J)="SChc:IPW:-:<NoVis>^Def:T:-:<NoVis>:"
           ENDIF
          ELSE IF (PROB(I,J).LT.55.) THEN
           IF (THUNDER(I,J).EQ.0.) THEN
            STRING(I,J)="Chc:IPW:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.1.) THEN
            STRING(I,J)="Chc:IPW:-:<NoVis>^SChc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.2.) THEN
            STRING(I,J)="Chc:IPW:-:<NoVis>^Chc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.3.) THEN
            STRING(I,J)="Chc:IPW:-:<NoVis>^Lkly:T:-:<NoVis>:"
           ELSE
            STRING(I,J)="Chc:IPW:-:<NoVis>^Def:T:-:<NoVis>:"
           ENDIF
          ELSE IF (PROB(I,J).LT.75.) THEN
           IF (THUNDER(I,J).EQ.0.) THEN
            STRING(I,J)="Lkly:IPW:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.1.) THEN
            STRING(I,J)="Lkly:IPW:-:<NoVis>^SChc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.2.) THEN
            STRING(I,J)="Lkly:IPW:-:<NoVis>^Chc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.3.) THEN
            STRING(I,J)="Lkly:IPW:-:<NoVis>^Lkly:T:-:<NoVis>:"
           ELSE
            STRING(I,J)="Lkly:IPW:-:<NoVis>^Def:T:-:<NoVis>:"
           ENDIF
          ELSE
           IF (THUNDER(I,J).EQ.0.) THEN
            STRING(I,J)="Def:IPW:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.1.) THEN
            STRING(I,J)="Def:IPW:-:<NoVis>^SChc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.2.) THEN
            STRING(I,J)="Def:IPW:-:<NoVis>^Chc:T:-:<NoVis>:"
           ELSE IF (THUNDER(I,J).EQ.3.) THEN
            STRING(I,J)="Def:IPW:-:<NoVis>^Lkly:T:-:<NoVis>:"
           ELSE
            STRING(I,J)="Def:IPW:-:<NoVis>^Def:T:-:<NoVis>:"
           ENDIF
         ENDIF
        ELSE
           STRING(I,J)="<NoCov>:<NoWx>:<NoInten>:<NoVis>:"
           GRIDWX(I,J)=0.
        ENDIF
       ENDDO
       ENDDO

       TRACK(1)="<NoCov>:<NoWx>:<NoInten>:<NoVis>:"
       do L=2,200
        TRACK(L)="empty"
       ENDDO

       START=2
       DO J=1,JM
       DO I=1,IM
         TEMP=STRING(I,J)
         DO L=1,200
          IF (TEMP.EQ.TRACK(L)) GOTO 444 
         ENDDO
         TRACK(START)=TEMP
         START=START+1
 444     CONTINUE
       ENDDO
       ENDDO       
Cjtm   do L=1,200
Cjtm    print *, 'track ', L, track(l) 
Cjtm   enddo
       do J=1,JM
       do I=1,IM
        do L=1,200 
         if (STRING(I,J).EQ.TRACK(L)) THEN 
           GRIDWX(I,J)=L*1.0
           goto 555
         endif
        enddo
 555    continue
       ENDDO   
       ENDDO      

       do I=765,775
       DO J=300,310
        print *, 'string test ', STRING(I,J), GRIDWX(I,J)
       ENDDO
       ENDDO

       RETURN
       END
