cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  Subroutine snow: compute 3/6/12/24 hour accumulated snow
c  mean/sprea/probability based on Jun Du's old version
c  Note: mean is "unconditional mean", that is, 0 snow also
c        is taken into account in mean computation
c
c  Author: Binbin Zhou, Agu. 8, 2005
c
c  Modification history: 
c  May 17, 2011, B. Zhou and Jun Du: Change water:snow ratio 
c                from 1:10 to 1:snowr, where snowr denpends on T2m 
c  Dec 15, 2011, B. Zhou and Jun Du: Introduced six PBL temp 
c                so that snowr can be dependent on surface layer T
c
c  Input: nv, itime, i00, precip, jf, iens, interval
c  output: derv_mn,  derv_pr
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
   	subroutine snow(nv,itime,i00,rawdata,precip,ice,snw,jf,iens,
     + interval,loutput,derv_mn,derv_sp,derv_pr,snowmax,snowmin) 

         include 'parm.inc'

C for variable table:
        Integer numvar, nderiv
        Character*4 vname(maxvar)
        Integer k5(maxvar), k6(maxvar)
        Character*1 Msignal(maxvar), Psignal(maxvar)
        Integer Mlvl(maxvar), MeanLevel(maxvar,maxmlvl)
        Integer Plvl(maxvar), ProbLevel(maxvar,maxplvl)
        Integer Tlvl(maxvar)
        Character*1 op(maxvar)
        Real    Thrs(maxvar,maxtlvl)
                                                                                                                                                                
c    for derived variables
        Character*4 dvname(maxvar)
        Integer dk5(maxvar), dk6(maxvar)
        Character*1 dMsignal(maxvar), dPsignal(maxvar)
        Integer dMlvl(maxvar), dMeanLevel(maxvar,maxmlvl)
        Integer dPlvl(maxvar), dProbLevel(maxvar,maxplvl)
        Character*1 dop(maxvar)
        Integer dTlvl(maxvar)
        Real    dThrs(maxvar,maxtlvl)
        Integer MPairLevel(maxvar,maxmlvl,2)
        Integer PPairLevel(maxvar,maxplvl,2)
                                                                                                                                                                
        common /tbl/numvar,
     +              vname,k5,k6,Mlvl,Plvl,Tlvl,
     +              MeanLevel,ProbLevel,Thrs,
     +              Msignal,Psignal,op
                                                                                                                                                                
        common /dtbl/nderiv,
     +              dvname,dk5,dk6,dMlvl,dPlvl,dTlvl,
     +              dMeanLevel,dProbLevel,dThrs,
     +              dMsignal,dPsignal,MPairLevel,PPairLevel,dop


        INTEGER, intent(IN) :: nv,itime,i00,jf,iens,interval,loutput
        REAL,dimension(jf,iens,numvar,maxmlvl),intent(IN) :: rawdata
        REAL,dimension(jf,iens,loutput),intent(IN) :: precip
        REAL,dimension(jf,iens,loutput),intent(IN) :: ice
        REAL,dimension(jf,iens,loutput),intent(IN) :: snw
        REAL,dimension(jf,nderiv,maxmlvl),intent(INOUT) :: derv_mn
        REAL,dimension(jf,nderiv,maxmlvl),intent(INOUT) :: derv_sp
        REAL,dimension(jf,nderiv,maxplvl,maxtlvl),intent(INOUT) :: 
     +               derv_pr
        REAL,dimension(jf,maxmlvl),intent(INOUT) :: snowmax
        REAL,dimension(jf,maxmlvl),intent(INOUT) :: snowmin

        INTEGER               :: tick     ! counter for counting previous times           
        REAL,dimension(iens)  :: apoint
        REAL                  :: amean,aspread,aprob

        REAL T2mPoint(iens)
        REAL Tref(iens)
        INTEGER ID_T2m method
        real Tpbl_layer(iens,7),Tpbl_mbr(7)
     
        method=1  !GM
c       method=2  !SPC 
c       method=3  !10:1

        ID_T2M = index_table(k5,k6,11,105,maxvar)     !Search for 2m temperature
        ID_Tpbl= index_table_var(vname,k5,k6,11,116,'Tpbl',numvar) !Search for PBL Ts

        if(ID_T2m.gt.0 .and. ID_Tpbl.gt.0 ) then

             do lv=1,dMlvl(nv)                                  !for accumulated hours
               if(itime.ge.dMeanLevel(nv,lv)) then

                 do igrid = 1,jf

C PBL Ts: 2m & 0-30, 30-60, 60-90, 90-120, 120-150 and 150-180mb AGL
                          T2mPoint=rawdata(igrid,:,ID_T2m,1)
                   Tpbl_layer(:,1)=rawdata(igrid,:,ID_T2m,1)
                   do k=2,7
                    Tpbl_layer(:,k)=rawdata(igrid,:,ID_Tpbl,k-1)
                   end do

        if(method.eq.1) then
c Use T2m only
                   Tref=Tpbl_layer(:,1)
        endif
c Use the warmest T in the layer from surface to 180mb AGL
        if(method.eq.2) then
                   do i=1,iens
                    Tpbl_mbr(:)=Tpbl_layer(i,:)
                    Tref(i)=maxval(Tpbl_mbr)
                   end do
        endif

c Find snowfall amount
                   apoint = 0.
                   tick = dMeanLevel(nv,lv)/interval - 1
                   do while ( tick .ge. 0 )
                     do irun = 1, iens

c Use T2m only for the modified Geoff Manikin's method (Jun Du)
        if(method.eq.1) then
                      if((T2mPoint(irun)-273.15).le.5.0) then
c GM's original formula:
c                      snowr=(273.15-Tref(irun))+8.0 
c Modified by Jun Du by multipling a slope to reduce high bias in cold T
c                      slope=tan(27.0/180.0*3.14159)
                       slope=0.5
                       snowr=slope*(273.15-Tref(irun))+8.0 
                       if(snowr.gt.28.0) snowr=28.0
c                      if(snowr.gt.20.0) snowr=20.0
                      else
                       snowr=0.0
                      endif 
        endif
c Use the warmest T in the layer from surface to 180mb AGL for the SPC method (Jun Du)
        if(method.eq.2) then
                      if((T2mPoint(irun)-273.15).le.5.0) then
                       snowr=1000.0/(100.0+6.0*(Tref(irun)-273.15))
                       if(snowr.lt.0.0.or.snowr.gt.40.0) snowr=40.0
                      else
                       snowr=0.0
                      endif
        endif
c Use fixed 10:1 ratio
        if(method.eq.3) then
                      snowr=10.0
        endif

                      snowx=ice(igrid,irun,i00-tick)+
     +                      snw(igrid,irun,i00-tick)
                      apoint(irun)=apoint(irun)+snowx*                    !consider ice|snw > 0 but case <1 
     +                      snowr*precip(igrid,irun,i00-tick)              

                     end do   
                      
                     tick = tick - 1

                   end do   !end of while

                   call getmean(apoint,iens,amean,aspread)
                   derv_mn(igrid,nv,lv) = amean
                   derv_sp(igrid,nv,lv) = aspread
                   snowmax(igrid,lv) = maxval(apoint)
                   snowmin(igrid,lv) = minval(apoint)

                 end do

               end if
             end do

             do lv=1,dPlvl(nv)
              if(itime.ge.dProbLevel(nv,lv)) then

                do lt =1,dTlvl(nv)
                  do igrid = 1, jf

                    T2mPoint=rawdata(igrid,:,ID_T2m,1)
                    apoint = 0.

                    tick = dProbLevel(nv,lv)/interval - 1
                    do while ( tick .ge. 0 )
                     do irun = 1, iens
                      snowr=(273.15-T2mPoint(irun))+8.0

                      snowx=ice(igrid,irun,i00-tick)+
     +                      snw(igrid,irun,i00-tick)
                      apoint(irun)=apoint(irun)+snowx*                    !consider ice|snw > 0 but case <1
c     +                      10.0*precip(igrid,irun,i00-tick)
     +                      snowr*precip(igrid,irun,i00-tick)

                     end do
                     tick = tick - 1
                    end do

                    if(trim(dop(nv)).ne.'-') then
                     thr1 = dThrs(nv,lt)
                     thr2 = 0.
               call getprob(apoint,iens,thr1,thr2,dop(nv),aprob)
                     derv_pr(igrid,nv,lv,lt)=aprob
                    else
                     if(lt.lt.dTlvl(nv)) then
                       thr1 = dThrs(nv,lt)
                       thr2 = dThrs(nv,lt+1)
               call getprob(apoint,iens,thr1,thr2,dop(nv),aprob)
                       derv_pr(igrid,nv,lv,lt)=aprob
                     end if
                    end if
                   
                  end do
                end do

              end if
             end do

           end if   !end if of ID_T2m

           return
           end
