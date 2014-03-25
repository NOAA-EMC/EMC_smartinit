cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     subroutine get_mxp: compute max,min,mode, 10,25,50, and 90% mean products
c     Author: Binbin Zhou, Apr. 7, 2011, based on Jun Du's code
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
   	subroutine get_mxp (nv,rawdata, jf, iens, mxp8 )

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

        common /tbl/numvar,
     +              vname,k5,k6,Mlvl,Plvl,Tlvl,
     +              MeanLevel,ProbLevel,Thrs,
     +              Msignal,Psignal,op

c   for max,min,10,25,50,90% mean products
        Character*4 qvname(maxvar)
        Integer qk5(maxvar), qk6(maxvar)
        Character*1 qMsignal(maxvar)
        Character*3 qMn(maxvar)
        Integer qMlvl(maxvar), qMeanLevel(maxvar,maxmlvl)


        common /qtbl/nmxp,
     +              qvname,qk5,qk6,qMlvl,
     +              qMeanLevel,qMsignal


        INTEGER, intent(IN) :: nv, jf, iens
        REAL,dimension(jf,iens,numvar,maxmlvl),intent(IN) :: rawdata
        REAL,dimension(jf,nmxp,maxmlvl,8),intent(INOUT) :: mxp8

        real x(iens)

        kv5=qk5(nv)
        kv6=qk6(nv)

        ID_x = index_table(k5,k6,kv5,kv6,maxvar)      !search index of direct variable in the table



          if (ID_x.gt.0 ) then

             do lv=1,qMlvl(nv)                       !for all pairs of levels

                L1=index_int_array(MeanLevel(ID_x,:),           !search index of 1st level from
     +                     qMeanlevel(nv,lv),maxmlvl)           !   ID_x-th variable's MeanLevel
                write(*,*) 'In mxp: L1=',L1
                if (L1.eq.0) then
                  write(*,*) qMeanlevel(nv,lv), 'not found'
                  STOP 113
                end if
 
                do igrid = 1,jf
                  x=rawdata(igrid,:,ID_x,L1)
c                 if(igrid.eq.137) then
c                  write(*,*)'In get_mxp:',qvname(nv),'lv=',lv
c                  write(*,'(i8,21f8.1)') igrid, x
c                 end if
                  call mean_median(iens,x,xm,xmed)
                  call probability(x,iens,vmin,vmax,p10,p25,p50,p75,p90)
                  mxp8(igrid,nv,lv,1)=vmin
                  mxp8(igrid,nv,lv,2)=vmax
                  mxp8(igrid,nv,lv,3)=3*p50-2*xm

                  if(kv5.eq.61.or.kv5.eq.51.or.kv5.eq.52) then
                   if (mxp8(igrid,nv,lv,3).lt.0.0)        then    !Jun: to deal with negative mod
                       mxp8(igrid,nv,lv,3)=p50
                   end if
                  end if

                  mxp8(igrid,nv,lv,4)=p10
                  mxp8(igrid,nv,lv,5)=p25
                  mxp8(igrid,nv,lv,6)=p50
                  mxp8(igrid,nv,lv,7)=p75
                  mxp8(igrid,nv,lv,8)=p90
c                  if(igrid.eq.137) then
c              write(*,'(i7,8f8.1)')igrid,(mxp8(igrid,nv,lv,kv),kv=1,8)
c                  end if
                end do

              end do

           end if

           return
           end
