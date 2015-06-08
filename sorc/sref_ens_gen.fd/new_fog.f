


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     subroutine new_fog: compute fog LWC according to the asymptotic analysis 
c                         of radiation fog theory but adding advection term
c     
c     Author: Binbin Zhou, Aug 18 , 2010 
c     Modification: 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        subroutine new_fog(nv,itime,i00,t4cooling,t2m4cooling,
     +     rawdata,jf,im,jm,dx,dy,iens,interval,loutput, 
     +     derv_mn,derv_sp,derv_pr,LWCfield)

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
 
               
        REAL,dimension(jf,iens,loutput,14),intent(IN) ::t4cooling
        REAL,dimension(jf,iens,loutput),intent(IN)    ::t2m4cooling
        REAL,dimension(jf,iens,loutput),intent(INOUT) ::LWCfield

        real apoint(iens),FOGapoint(iens)
        real u10,v10,hsfc,up(14),vp(14),hp(14),
     +       tp(14,2),t2(2),rh2,rhp(14),lwc,lwc1(jf,iens)

        real qw(jf),qw2d(im,jm),qadv(jf,iens),
     +       qadv2d(im,jm),u2d(im,jm),v2d(im,jm)
        real dx,dy,ddx,ddy,q_adv

           ID_U10=index_table(k5,k6,33,105,numvar)    !search index of direct variable u10 in the table
           ID_V10=index_table(k5,k6,34,105,numvar)    !search index of direct variable v10 in the table
           ID_U=index_table_var(vname,k5,k6,33,100,'ULWS',numvar) !search index of direct variable u in the table
           ID_V=index_table_var(vname,k5,k6,34,100,'VLWS',numvar) !search index of direct variable v in the table
           ID_RH2=index_table(k5,k6,52,105,numvar)     !search index of direct variable RH 2m in the table
           ID_RH=index_table_var(vname,k5,k6,52,100,'RHFG',numvar) !search index of direct variable profile RH in the table
           ID_CLDB=index_table(k5,k6,7,2,maxvar)      !search index of cloud base heightin the table
           ID_CLDT=index_table(k5,k6,7,3,maxvar)      !search index of cloud cloud heightin the table
 
           !Note: there are other U,V at 850,550 and 250 mb for jet stream. So can not search ULWS,VLWS. 
           !So additional info vname to further search wind at 14 pressure levels  
                                  
           ID_H=index_table_var(vname,k5,k6,7,100,'HLWS',numvar)      !searcg index of direct variable height in tha table
           ID_SF=index_table(k5,k6, 7, 1,numvar)      !searcg index of direct variable hsfc in tha table


            write(*,*) 'In new_fog=', ID_U10,ID_V10,
     +  ID_U,ID_V,ID_H,ID_SF,ID_RH2, ID_RH,
     +    ID_CLDB,ID_CLDT
           write(*,*) jf,iens,interval,loutput,i00

        if (ID_RH  .gt.0 .and.
     +        ID_U10 .gt.0 .and. ID_V10 .gt.0) then    !compute advection LWC term with sepecic humidity q to estimate

           ddx=dx/1000.   !m-->km        
           ddy=dy/1000.

           qadv=0.

           do k=1,iens  

            qadv2d=0.

            do igrid = 1,jf
             RH=rawdata(igrid,k,ID_RH2,1)/100.0
             t=t2m4cooling(igrid,k,i00)
c             tt=7.45*t/(235.0 + t)
c             es = 6.10 *10 ** tt                        !es:saturated vapor pressure, qw: specific humiduty
C        Use WMO recommended formulation (2008)
C   Guide to Metteorological Instruments and Methods of
C   Observation (CIMO Guide):

          if( t.ge.0.0) then
            tt=17.62*t/(243.12 + t)     !over liquid water
          else
            tt=22.46*t/(272.62 + t)     !over ice, assume no supercooled water
          end if

          es = 6.112 *2.71828 ** tt                     !es:saturated vapor pressure

             qw(igrid)=622.0*es*RH/1000.0               !qw=622*es*RH/P    (g/kg)
            end do
 
            do j=1,jm
             do i=1,im
              ij=(j-1)*im + i 
              qw2d(i,j)=qw(ij)+LWCfield(ij,k,i00-1)
              u2d(i,j)=rawdata(ij,k,ID_U10,1)
              v2d(i,j)=rawdata(ij,k,ID_V10,1)
             end do
            end do
 
            do j=2,jm-1                                            !general upwind scheme
             do i=2,im-1  
              if(u2d(i,j).ge.0.0.and.v2d(i,j).ge.0.0) then      
                 qadv2d(i,j) = 
     +            -u2d(i,j)*(qw2d(i,j)-qw2d(i-1,j))/ddx  
     +            -v2d(i,j)*(qw2d(i,j)-qw2d(i,j-1))/ddy
         else if(u2d(i,j).gt.0.0.and.v2d(i,j).lt.0.0) then
                 qadv2d(i,j) =
     +            -u2d(i,j)*(qw2d(i,j)-qw2d(i-1,j))/ddx
     +            -v2d(i,j)*(qw2d(i,j+1)-qw2d(i,j))/ddy
         else if(u2d(i,j).lt.0.0.and.v2d(i,j).lt.0.0) then
                  qadv2d(i,j) =
     +            -u2d(i,j)*(qw2d(i+1,j)-qw2d(i,j))/ddx
     +            -v2d(i,j)*(qw2d(i,j+1)-qw2d(i,j))/ddy
         else if(u2d(i,j).lt.0.0.and.v2d(i,j).gt.0.0) then
                  qadv2d(i,j) =
     +             -u2d(i,j)*(qw2d(i+1,j)-qw2d(i,j))/ddx
     +             -v2d(i,j)*(qw2d(i,j)-qw2d(i,j-1))/ddy
              end if
             end do
            end do

            do j=1,jm
             do i=1,im
              ij=(j-1)*im + i
              qadv(ij,k)=qadv2d(i,j)
             end do
            end do
 
          end do                                                      

        end if

        if (ID_U10 .gt.0 .and. ID_V10 .gt.0 .and.
     +          ID_U .gt.0 .and.   ID_V .gt.0 .and. 
     +          ID_H .gt.0 .and.  ID_SF .gt.0 .and.
     +          ID_RH2.gt.0. and. ID_RH .gt.0 .and.
     +          ID_CLDB.gt.0. and. ID_CLDT.gt.0 ) then

             write(*,*) 'dMlvl(nv)=',dMlvl(nv)

             do lv=1,dMlvl(nv)                       !for all  levels

              do igrid = 1,jf
                          
               do i=1,iens
 
               
                 u10= rawdata(igrid,i,ID_U10,1)
                 v10= rawdata(igrid,i,ID_V10,1)
                 hsfc=rawdata(igrid,i,ID_SF, 1)
                 rh2= rawdata(igrid,i,ID_RH2,1)
                 t2(1) = t2m4cooling (igrid,i,i00-1)    !previous time step
                 t2(2) = t2m4cooling (igrid,i,i00)      !this time step
                 cldb = rawdata(igrid,i,ID_CLDB,1)-hsfc !cloudbase above ground
                 cldt = rawdata(igrid,i,ID_CLDT,1)-hsfc !cloud top above ground
                 q_adv=qadv(igrid,i)

                 do k=1,14
                  up(k)=rawdata(igrid,i,ID_U,k)
                  vp(k)=rawdata(igrid,i,ID_V,k)
                  hp(k)=rawdata(igrid,i,ID_H,k)
                  rhp(k)=rawdata(igrid,i,ID_RH,k)  
                  tp(k,1)=t4cooling(igrid,i,i00-1,k)   !previous time step
                  tp(k,2)=t4cooling(igrid,i,i00,k)     !this time step
                 end do
                
                 lwc=0.0
                 call get_new_fog(up,vp,hp,u10,v10,hsfc,
     +            rhp,rh2,t2,tp,interval,14,cldt,cldb,q_adv,
     +            lwc)
                 FOGapoint(i)=lwc             
                 lwc1(igrid,i)=lwc
c                 LWCfield(igrid,i,i00)=lwc   !this array has error

               end do 


                call getmean(FOGapoint,iens,amean,aspread)
                  derv_mn(igrid,nv,lv)=amean
                  derv_sp(igrid,nv,lv)=aspread

c          write(*,'(i8,22f6.2)') igrid,FOGapoint,amean

                end do  

              end do


              do lv=1,dPlvl(nv)

                do lt = 1, dTlvl(nv)
                              
                 do igrid = 1,jf

                    FOGapoint(:)=lwc1(igrid,:)

                    if(trim(dop(nv)).ne.'-') then
                     thr1 = dThrs(nv,lt)
                     thr2 = 0.
               call getprob(FOGapoint,iens,thr1,thr2,dop(nv),aprob)
                     derv_pr(igrid,nv,lv,lt)=aprob
                    else
                     if(lt.lt.dTlvl(nv)) then
                       thr1 = dThrs(nv,lt)
                       thr2 = dThrs(nv,lt+1)
               call getprob(FOGapoint,iens,thr1,thr2,dop(nv),aprob)
                       derv_pr(igrid,nv,lv,lt)=aprob
                     end if
                    end if

                 end do

                end do
              end do

           end if

           return
           end


