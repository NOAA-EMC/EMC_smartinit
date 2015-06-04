	subroutine pack_mxp(itime,eps,mxp8,
     +    jf,iens,iyr,imon,idy,ihr,gribid,lb,kgds,interval) 

C OUTPUT:
C KPDS(25) INTEGER*4
C ARRAY TO CONTAIN THE ELEMENTS OF THE PRODUCT DEFINITION SEC
C (VERSION 1)
C KPDS(1)  - ID OF CENTER
C KPDS(2)  - MODEL IDENTIFICATION (SEE "GRIB" TABLE 1)
C KPDS(3)  - GRID IDENTIFICATION (SEE "GRIB" TABLE 2)
C KPDS(4)  - GDS/BMS FLAG
C            BIT       DEFINITION
C            25        0 - GDS OMITTED
C                      1 - GDS INCLUDED
C            26        0 - BMS OMITTED
C                      1 - BMS INCLUDED
C            NOTE:- LEFTMOST BIT = 1,
C                   RIGHTMOST BIT = 32
C KPDS(5)  - INDICATOR OF PARAMETER (SEE "GRIB" TABLE 5)
C KPDS(6)  - TYPE OF LEVEL (SEE "GRIB" TABLES 6 & 7)
C KPDS(7)  - HEIGHT,PRESSURE,ETC  OF LEVEL
C KPDS(8)  - YEAR INCLUDING CENTURY
C KPDS(9)  - MONTH
C KPDS(11) - HOUR OF DAY
C KPDS(12) - MINUTE OF HOUR
C KPDS(13) - INDICATOR OF FORECAST TIME UNIT (SEE "GRIB"
C            TABLE 8)
C KPDS(14) - TIME 1               (SEE "GRIB" TABLE 8A)
C KPDS(15) - TIME 2               (SEE "GRIB" TABLE 8A)
C KPDS(16) - TIME RANGE INDICATOR (SEE "GRIB" TABLE 8A)
C KPDS(17) - NUMBER INCLUDED IN AVERAGE
C KPDS(18) - EDITION NR OF GRIB SPECIFICATION
C KPDS(19) - VERSION NR OF PARAMETER TABLE
C
C KPDS(20)  - NR MISSING FROM AVERAGE/ACCUMULATION
C KPDS(21)  - CENTURY OF REFERENCE TIME OF DATA
C KPDS(22)  - UNITS DECIMAL SCALE FACTOR
C KPDS(23)  - SUBCENTER NUMBER
C KPDS(24)  - PDS BYTE 29, FOR NMC ENSEMBLE PRODUCTS
C         128 IF FORECAST FIELD ERROR
C          64 IF BIAS CORRECTED FCST FIELD
C          32 IF SMOOTHED FIELD
C         WARNING: CAN BE COMBINATION OF MORE THAN 1
C KPDS(25)  - PDS BYTE 30, NOT USED
C KPDS(26-35)  - RESERVED
C KPDS(36-N)   - CONSECUTIVE BYTES EXTRACTED FROM PROGRAM
C                DEFINITION SECTION (PDS) OF GRIB MESSAGE
C REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM
C
C$$$
C

       include 'parm.inc'

       dimension kpds(25)
       dimension kens(5),kprob(2),xprob(2),kclust(16),kmembr(80)
       character gds(42)
       character*20 maxout,minout,modout,p10out,p25out,p50out,
     +                 p75out, p90out
       character*10 date
       character*4  eps     !ensemble prediction system, either sref or gens 

 
        INTEGER,intent(IN) :: itime,jf, iens, iyr,imon,idy,ihr, 
     +                        gribid, kgds(25)
        REAL,dimension(jf,nmxp,maxmlvl,8),intent(IN) :: mxp8
        logical, dimension(jf),intent(IN)            :: lb 

        REAL var(jf)      

c   for max,min,10,25,50,90% mean products
        Character*4 qvname(maxvar)
        Integer qk5(maxvar), qk6(maxvar)
        Character*1 qMsignal(maxvar)
        Character*3 qMn(maxvar)
        Integer qMlvl(maxvar), qMeanLevel(maxvar,maxmlvl)

        common /qtbl/nmxp,
     +              qvname,qk5,qk6,qMlvl,
     +              qMeanLevel,qMsignal



            
      if(itime.lt.100) then                                                                                                                           
       write(date,'(i2.2)') itime
      else
       write(date,'(i3.3)') itime
      end if

      write(*,*) 'packing mxp ....'
      write(*,*) itime, jf,iens,nmxp    

      minout='min.'//eps//'.f' // trim(date)
      maxout='max.'//eps//'.f' // trim(date)
      modout='mod.'//eps//'.f' // trim(date)
      p10out='p10.'//eps//'.f' // trim(date)
      p25out='p25.'//eps//'.f' // trim(date)
      p50out='p50.'//eps//'.f' // trim(date)
      p75out='p75.'//eps//'.f' // trim(date)
      p90out='p90.'//eps//'.f' // trim(date)


      call baopen(10,minout,ierr)
      call baopen(20,maxout,ierr)
      call baopen(30,modout,ierr)
      call baopen(40,p10out,ierr)
      call baopen(50,p25out,ierr)
      call baopen(60,p50out,ierr)
      call baopen(70,p75out,ierr)
      call baopen(80,p90out,ierr)

 
      kpds(1)=7
      kpds(2)=113
      kpds(3)=gribid
      kpds(4)=0
c     kpds(5)=variable dependent
c     kpds(6)=variable dependent
c     kpds(7)=variable dependent
      kpds(8)=iyr-(iyr/100)*100
      kpds(9)=imon
      kpds(10)=idy
      kpds(11)=ihr
      kpds(12)=0
      kpds(13)=1
c     kpds(14)=variable dependent
c     kpds(15)=variable dependent
c     kpds(16)=variable dependent
      kpds(17)=-1
      kpds(18)=1
      kpds(19)=2
      kpds(20)=255
      kpds(21)=21
c     kpds(22)=variable dependent (units decimal scale factor/precision)
      kpds(23)=2
      kpds(24)=128
      kpds(25)=-1

      kens(1)=1           !: OCT 41
      kens(2)=5           !: OCT 42
      kens(3)=0           !: OCT 43
c     kens(4)=variable dependent       !: OCT 44
      kens(5)=255         !: OCT 45    !: Original resolution retained
      kclust(1)=iens      !: OCT 61

c       kprob(1)=variable dependent         !: OCT 46
c       kprob(2)=variable dependent         !: OCT 47
c       xprob(1)=variable dependent        !: OCT 48-51
c       xprob(2)=variable dependent        !: OCT 52-55

cccccc  write max,min,mode, 10,25,50,75,90% mean variables into 7 grib files
                                                                                                                            
        kprob(1) = -1         !: OCT 46
        kprob(2) = -1         !: OCT 47
        xprob(1) = -1.        !: OCT 48-51
        xprob(2) = -1.        !: OCT 52-55
        kpds(14) = itime
        kpds(15) = 0
        kpds(16) = 0
        kpds(22) = 2

       do 201 nv = 1, nmxp
        if(trim(qMsignal(nv)).eq.'Q') then
           if(qk5(nv).eq.24.or.qk5(nv).eq.11.or.qk5(nv).eq.33.or.
     +      qk5(nv).eq.34.or.qk5(nv).eq.7 .or.qk5(nv).eq.32) then
            kpds(22) = 2
           else if (qk5(nv).eq.41.or.qk5(nv).eq.153) then
            kpds(22) = 6
           else if (qk5(nv).eq.51) then     !JunDu, 07/19/2008
            kpds(22) = 4
           else
            kpds(22) = 2
           end if
                                                                                                                            
           kpds(5) = qk5(nv)
           kpds(6) = qk6(nv)

           write(*,*) 'packing ...kpds5,6=', kpds(5), kpds(6)
           do lv = 1, qMlvl(nv)
 
             kpds(7)  = qMeanLevel(nv,lv)
             
             if(qk5(nv).eq.131.and.qk6(nv).eq.101) then  !special case: kpds(5)(6)(7) in model
                kpds(5) = 24                           !       grib files are different from 
                kpds(6) = 116                          !       those of ensemble posted grib
                kpds(7) = 7680                         !       files set by Jun Du
             end if
                              
             kens(4) = 2

             if(qk5(nv).eq.61.and.qk6(nv).eq.1) then    !for precip
              kpds(14)=itime-interval
              kpds(15)=itime
              kpds(16)= 4 
             else
              kpds(14)=itime
              kpds(15)=0
              kpds(16)=0
             end if

             do k=1,8
              ngrb=10*k
              var=mxp8(:,nv,lv,k)

              if(kpds(5).eq.52) then              !RH max/min range control
               do i=1,jf
                if(var(i).lt.0.) var(i)=0.
                if(var(i).gt.100.) var(i)=100.
               end do
              end if
              if(kpds(5).eq.51) then             !q min control
               do i=1,jf
                if(var(i).lt.0.) var(i)=0.
               end do
              end if
              if(kpds(5).eq.61) then             !precip min control
               do i=1,jf
                if(var(i).lt.0.) var(i)=0.
               end do
              end if


              call putgbex(ngrb,jf,kpds,kgds,kens,kprob,xprob,
     &             kclust,kmembr,lb,var,iret)
             end do
           end do
         end if
201      continue      
  

      call baclose(10,ierr)
      call baclose(20,ierr)
      call baclose(30,ierr)
      call baclose(40,ierr)
      call baclose(50,ierr)
      call baclose(60,ierr)
      call baclose(70,ierr)

      return
      end

