cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     subroutine LLWS: compute low level wind shear 
c     
c     Author: Binbin Zhou, Aug, 7, 2005
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
   	subroutine llws (nv,rawdata, jf, iens,  
     +             derv_mn, derv_sp, derv_pr)

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


        INTEGER, intent(IN) :: nv, jf, iens
        REAL,dimension(jf,iens,numvar,maxmlvl),intent(IN) :: rawdata
        REAL,dimension(jf,nderiv,maxmlvl),intent(INOUT) :: derv_mn
        REAL,dimension(jf,nderiv,maxmlvl),intent(INOUT) :: derv_sp
        REAL,dimension(jf,nderiv,maxplvl,maxtlvl),intent(INOUT) :: 
     +               derv_pr

        real apoint(iens),LLWSapoint(iens),llws1(jf,iens)
        real ws,u10,v10,hsfc,up(14),vp(14),hp(14)

           ID_U10 = index_table(k5,k6,33,105,numvar)    !search index of direct variable u10 in the table
           ID_V10 = index_table(k5,k6,34,105,numvar)    !search index of direct variable v10 in the table
           ID_U = index_table_var(vname,k5,k6,33,100,'ULWS',numvar)      !search index of direct variable u in the table
           ID_V = index_table_var(vname,k5,k6,34,100,'VLWS',numvar)      !search index of direct variable v in the table
           !Note: there are other U,V at 850,550 and 250 mb for jet stream. So can not search ULWS,VLWS. 
           !So additional info vname to further search wind at 14 pressure levels for LLWS                                             
           ID_H = index_table_var(vname,k5,k6, 7,100,'HLWS',numvar)      !searcg index of direct variable height in tha table
           ID_SF = index_table(k5,k6, 7, 1,numvar)      !searcg index of direct variable hsfc in tha table


            write(*,*) 'In LLWS=', ID_U10,ID_V10,
     +  ID_U,ID_V,ID_H,ID_SF


          if (ID_U10 .gt.0 .and. ID_V10 .gt.0 .and.
     +          ID_U .gt.0 .and.   ID_V .gt.0 .and. 
     +          ID_H .gt.0 .and.  ID_SF .gt.0 ) then

             do lv=1,dMlvl(nv)                       !for all  levels

            write(*,*) 'In LLWS=', ID_U10,ID_V10,
     +  ID_U,ID_V,ID_H,ID_SF

              do igrid = 1,jf

               do i=1,iens
                 u10= rawdata(igrid,i,ID_U10,1)
                 v10= rawdata(igrid,i,ID_V10,1)
                 hsfc=rawdata(igrid,i,ID_SF, 1)
                 do k=1,14
                  up(k)=rawdata(igrid,i,ID_U,k)
                  vp(k)=rawdata(igrid,i,ID_V,k)
                  hp(k)=rawdata(igrid,i,ID_H,k)
                 end do
  
                 call get_llws(up,vp,hp,u10,v10,hsfc,jf,14,ws)

                  LLWSapoint(i)=ws
                  llws1(igrid,i)=ws

                end do 

                call getmean(LLWSapoint,iens,amean,aspread)
                  derv_mn(igrid,nv,lv)=amean
                  derv_sp(igrid,nv,lv)=aspread

                  if(igrid.eq.4300) then
                   write(*,*) 'LLWS=',amean,aspread
                  end if
                end do

              end do

              do lv=1,dPlvl(nv)

                do lt = 1, dTlvl(nv)
                              
                 do igrid = 1,jf

                    LLWSapoint(:)=llws1(igrid,:)

                    if(trim(dop(nv)).ne.'-') then
                     thr1 = dThrs(nv,lt)
                     thr2 = 0.
               call getprob(LLWSapoint,iens,thr1,thr2,dop(nv),aprob)
                     derv_pr(igrid,nv,lv,lt)=aprob
                    else
                     if(lt.lt.dTlvl(nv)) then
                       thr1 = dThrs(nv,lt)
                       thr2 = dThrs(nv,lt+1)
               call getprob(LLWSapoint,iens,thr1,thr2,dop(nv),aprob)
                       derv_pr(igrid,nv,lv,lt)=aprob
                     end if
                    end if

                 end do

                end do
              end do

           end if

           return
           end


      subroutine get_llws (up,vp,hp,u10,v10,hsfc,jf,lv,llws)
c         up,vp : u,v on pressure levels
c            hp : height of pressure levels
c       u10,v10 : 10m u and v
c          hsfc : sfc height

        INTEGER jf,lv
        REAL up(lv),vp(lv),hp(lv)
        REAL u10,v10,hsfc,llws


         z1 = 10. + hsfc
          if (z1 .lt. hp(1) ) then
            kp = 0
          else
            do k = 1,lv-1
             if(z1.ge.hp(k).and.z1.lt.hp(k+1)) then
               kp=k
             end if
            end do
          end if

          hz1 = hp(kp+1) - z1

          dh=0.0

          if((hz1+10.0).gt.609.6) then                    !609.6m=2000ft, hz1+10 means from sfc instead form 10m
            u2=u10 + (up(kp+1)-u10)*599.6/hz1
            v2=v10 + (vp(kp+1)-v10)*599.6/hz1
            z2=hsfc+609.6
          else
            do k=kp+1,lv-1
              dh=dh+hp(k+1)-hp(k)
              if((dh+hz1+10).gt.609.6) then  !ie, reach to 2000 feet
               z2=hsfc+609.6
               rt=(z2-hp(k))/(hp(k+1)-hp(k))
               u2=up(k)+(up(k+1)-up(k))*rt
               v2=vp(k)+(vp(k+1)-vp(k))*rt
               k2=k
               goto 609
              end if
            end do
          end if

609       llws=abs(sqrt((u2-u10)**2+(v2-v10)**2))
     &              /609.9
          llws=llws*1.943*609.9                             !unit--> knot/2000ft

     
       return
       end

