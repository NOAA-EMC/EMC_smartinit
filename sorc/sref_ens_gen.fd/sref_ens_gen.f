C$$$  MAIN PROGRAM DOCUMENTATION BLOCK $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C
C MAIN PROGRAM: SREF_ENS_GEN
C   PRGMMR: DU               ORG: NP21        DATE: 2002-01-02
C
C ABSTRACT: READS IN SREF GRIB AND OUTPUTS ENSEMBLE GRIB 
C           PRODUCTS. 
C
C PROGRAM HISTORY LOG:
c
c Old Version:
c 2001-07-10    Jun Du, initial program
c
c Generic Version: 
c
c 2005-08-01 Author: Binbin Zhou 
c
c       Re-organize the code structure and re-write the program in following features:
c
c        (1) The generic version dosn't fix GRIB# like 212 as old version. It can process
c            any GRIB#, and the domain (IM, JM) for the defined GRIB# is also flexible. 
c            As long as GRIB# is specified in 'filename', this program can automatically 
c            retrievs the domain (IM, JM, JF=IM*JM), then all arrays associated with this 
c            domain are dynamically allocated (on the fly)
c        (2) Ensemble members (IENS) are not fixed, but depends on the input file "filename'
c            in which, the all of soft linked file name are listed in order. The order and
c            number of files are changeable, the program can automatically allocate them
c        (3) The variables are changeable by using the 'variable.tbl' file, in which, two kinds of
c            variables are defined: (i) direct variables: read from GRIB files. Direct  
c            variables will also be computed for mean/spread an probability depending 
c            on the settings in each variable redcord (ii) derived variables: they are 
c            not read from GRIB files but need to do some derivation from direct variables.
c        (4) Derived variables need some algorithm to derive, Currently, only following derived 
c            variables are defined in this program:
c            (A) Thickness of two (pressure) levels: mean/spread/probability         
c            (B) 3hr, 6hr, 12hr, 24hr Accumulated precipitation: mean/spread/probability 
c            (C) 3hr, 6hr, 12hr, 24hr Accumulated snowfall: mean/spread/probability
c            (D) Precipitation type: probability
c            (E) Dominant precipitation: Mean
c            If want to process other derived product, user must provide their algorithms 
c            and then the computation code should be put into this program       
c        (5) User can request any kind of direct variables, either mean/spread or probability
c            by editing the 'variable.tbl' file (read README file for its gramma)
c 
c change log:
c 2006-01-12:Binbin Zhou
c             Add Geoff Manikin's algorithm to determine dominant precip type 
c 2006-03-01:Binbin Zhou
c             Modify wind spread computation by considering the wind directions, suggested 
c             by Jun Du            
c 2006-03-30: Jun Du: convert the unit from K to C for lifted index 
c
c 2006-04-30: Binbin Z. Add DTRA variance variables 
c
c 2006-05-15: Binbin Zhou
c             Modify to accept Beijing 2008 Olympic Game Grid#137
c
c 2006-07-21: Binbin Z. Add fog probability
c
c 2007-01-09: Binbin Z. Add freezing rain for GFS ensemble
c
c 2007-01-15: Binbin Z. Add Absolute vorticity for global ensemble since GFS has no absolute vorticity output
c 
c 2007-04-05: Binbin Z. Add High resolution WRF testing grid#137 
c
c 2007-10-09: Binbin Z. Add flight-restriction probability computation
c
c 2010-04-00: Binbin Zhou and Jun Du. Add hourly output
c
c 2011-04-07: Binbin Z. and Jun Du Add Max, Min, Mode, 10,25,50,75 and 90% mean products
c 2011-04-15: Binbin Z. Add cpat lightning products (based on David Bright's SPC program)
c 2011-04-18: Binbin Z. Add fire weather probability computation
c 2011-04-18: Binbin Z. Add LLWS code 
c 2011-05-05: Binbin Z. Add New fog code (based on Zhou and Ferrier 2008 algo.)
c 2011-05-20: Binbin Z. Change cptp to cptp_hrly_rgn3 as suggested by David B.
c 2011-06-10: Binbin Z. Add cptp_dryt based on David B.'s code (but dryt_hrly_rgn3 seems not good)
c 2011-06-20: Binbin Z. Add severe thunder storm potential based on David B's code
c 2011-06-28: Binbin Z. Add cluster mean calculation
c 2011-07-18: Binbin Z. and Jun Du Add two snow-liquid ratio schemes: Geoff Manikin's and 
c                                  SPC method (David Bright) with some modifications
c 2011-12-10: Jun Du: Added the 5km NDGD grid (initially called 259; now called 197 officially)
c 2012-04-25: Jun Du: Added 16km NA grid (g132)
c 2012-05-03: Binbin Z. Modified to run single forecast hour
c 2012-06-01: Jun Du: Added an ID "sys" to identify which ensemble system, sref(111) or gefs(222) or else
c
c$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$
C

       include 'parm.inc'

C  raw data
      real,allocatable,dimension(:,:,:)   :: precip                   !jf,iens, number of fcst output files
      real,allocatable,dimension(:,:,:)   :: mrk_ice                  !jf,iens, number of fcst output files
      real,allocatable,dimension(:,:,:)   :: mrk_frz                  !jf,iens, number of fcst output files
      real,allocatable,dimension(:,:,:)   :: mrk_snow                 !jf,iens, number of fcst output files
      real,allocatable,dimension(:,:,:,:) :: rawdata_mn, rawdata_pr   !jf,iens,maxvar, maxmlvl
      real,allocatable,dimension(:,:,:,:) :: t4cooling                !jf,iens,number of fcst output files,14
      real,allocatable,dimension(:,:,:)   :: t2m4cooling              !jf,iens,number of fcst output files
      real,allocatable,dimension(:,:,:)   :: LWCfield                 !jf,iens,number of fcst output files
      real,allocatable,dimension(:,:) :: prcp, m_ice,m_frz,m_snow
      real,allocatable,dimension(:)   ::t2m4fog
      real,allocatable,dimension(:,:) :: t4fog

C mean
      real,allocatable,dimension(:,:,:) :: vrbl_mn                    !jf, numvar,maxmlvl
      real,allocatable,dimension(:,:,:) :: derv_mn                    !jf, nderiv,maxmlvl

      real,allocatable,dimension(:,:)   :: prcpmax                    !jf, maxmlvl
      real,allocatable,dimension(:,:)   :: prcpmin                    !jf, maxmlvl
      real,allocatable,dimension(:,:)   :: snowmax                    !jf, maxmlvl
      real,allocatable,dimension(:,:)   :: snowmin                    !jf, maxmlvl
      real,allocatable,dimension(:,:)   :: frznmax                    !jf, maxmlvl
      real,allocatable,dimension(:,:)   :: frznmin                    !jf, maxmlvl


C spread

      real,allocatable,dimension(:,:,:) :: vrbl_sp                    !jf, numvar, maxmlvl
      real,allocatable,dimension(:,:,:) :: derv_sp                    !jf, nderiv, maxmlvl

      real,allocatable,dimension(:) :: ppt3_sp,ppt6_sp,ppt12_sp,
     *                                 ppt24_sp,s12_sp

C probability
      real,allocatable,dimension(:,:,:,:) :: vrbl_pr                   !jf, numvar, maxplvl, maxtlvl
      real,allocatable,dimension(:,:,:,:) :: derv_pr                   !jf, nderiv, maxplvl, maxtlvl

C New added: Mix,min, 10,25,50 and 90% mean products
      real,allocatable,dimension(:,:,:,:) :: mxp8                      !jf,numvar,maxmlvl,7

C others 
       logical, allocatable, dimension(:) ::      lb                   !jf
       character*19, allocatable, dimension(:) :: fhead
       real,allocatable,dimension(:) ::           var                  !jf
       real,allocatable,dimension(:) ::           apoint               !iens
       real,allocatable,dimension(:,:,:)  :: ptype_mn,ptype_pr         !jf,maxmlvl,4 

       real,allocatable,dimension(:,:,:) :: derv_dtra                  !jf,maxmlvl,8 for DTRA requests
       real,allocatable, dimension(:)    :: Hsfc                       !surface height for DTRA  
      

C for get grib size jf=im*jm
  
       integer iyr,imon,idy,ihr,ihh1,ihh2,sys
       character*50 gdss(400)
       integer IENS, GRIBID, kgdss(200), lengds,im,jm,km,jf,jff

       integer  interval, loutput

cc%%%%%%%  8. To be added if necessary ...........................
C original                                                                                                                                           
       dimension kpds(25),kgds(25)
       dimension kens(5),kprob(2),xprob(2),kclust(16),kmembr(80)
       character gds(42)
       character*13 mnout
       character*15 spout
       character*13 prout
       character*10 date
       character*2 pert2,cyc,fhr,fhr_1
       character*3 hr,pert
       character*19 fname,head

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
        Character*4 eps       
        Character*10 models     
        Character*200 oneline                                                                                                                                Integer nsubstr
        Character*20  substr(50)                
  
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
        
	Integer timestep, fnn(100)   
        Integer nfhr, pfog(14) 

c   for max,min,10,25,50,90% mean products
        Character*4 qvname(maxvar)
        Integer qk5(maxvar), qk6(maxvar)
        Character*1 qMsignal(maxvar)
        Character*3 qMn(maxvar)
        Integer qMlvl(maxvar), qMeanLevel(maxvar,maxmlvl)
        Integer, allocatable, dimension(:) :: missing

        common /tbl/numvar,
     +              vname,k5,k6,Mlvl,Plvl,Tlvl,
     +              MeanLevel,ProbLevel,Thrs,
     +              Msignal,Psignal,op
                                                                                                                                                                
        common /dtbl/nderiv,
     +              dvname,dk5,dk6,dMlvl,dPlvl,dTlvl,
     +              dMeanLevel,dProbLevel,dThrs,
     +              dMsignal,dPsignal,MPairLevel,PPairLevel,dop
 
        common /qtbl/nmxp,
     +              qvname,qk5,qk6,qMlvl,
     +              qMeanLevel,qMsignal
                                                                                                                                                               
       namelist/namin/iyr,imon,idy,ihr,ihh1,ihh2,sys


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Read table file to get wanted ensemble variable information
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        nunit=101
        open (nunit, file='variable.tbl', status='old')

        call readtbl(nunit)

        close(nunit)

        write(*,*) 'Check direct variable reading:'
        do i = 1, numvar
          write(*,*) vname(i),k5(i), k6(i),Msignal(i), Mlvl(i),
     +                 (MeanLevel(i,j),j=1,Mlvl(i)),
     +    Psignal(i), Plvl(i), (ProbLevel(i,j),j=1,Plvl(i)),
     +    op(i), Tlvl(i),   (Thrs(i,j),j=1,Tlvl(i))
        end do

        write(*,*) 'Check derived variable reading:'
        do i = 1, nderiv
         if(dk5(i).eq.7.and.dk6(i).eq.101) then
           write(*,*) dvname(i),dk5(i), dk6(i),dMsignal(i),dMlvl(i),
     +              (dMeanLevel(i,j),j=1,dMlvl(i)),
     +              (MPairLevel(i,j,1),j=1,dMlvl(i)),
     +              (MPairLevel(i,j,2),j=1,dMlvl(i)),
     +          dPsignal(i),dPlvl(i),(dProbLevel(i,j),j=1,dPlvl(i)),
     +              (PPairLevel(i,j,1),j=1,dPlvl(i)),
     +              (PPairLevel(i,j,2),j=1,dPlvl(i)),
     +              dop(i), dTlvl(i),   (dThrs(i,j),j=1,dTlvl(i))
         else
           write(*,*) dvname(i),dk5(i), dk6(i),dMsignal(i),dMlvl(i),
     +              (dMeanLevel(i,j),j=1,dMlvl(i)),
     +          dPsignal(i),dPlvl(i),(dProbLevel(i,j),j=1,dPlvl(i)),
     +             dop(i), dTlvl(i),   (dThrs(i,j),j=1,dTlvl(i))
         end if
        end do

        write(*,*) 'Check max,min,10,25,50,75,90% mean product reading:'
        do i = 1, nmxp
         write(*,*) qvname(i),qk5(i), qk6(i),qMsignal(i),qMlvl(i),
     +              (qMeanLevel(i,j),j=1,qMlvl(i))
        end do


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Read filename file to get # of ensemble available, grib#, vertical pressure level,
c    last forecast time, forecast outout interval and grib file heads, 
c    so that arrays can be dynamically allocatable
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       open(1, file='filename', status='old')
       read(1,*) IENS,GRIBID,KM,timestep,interval,nfhr     !nfhr new added to consider cluster case
       read(1,201) (fnn(i),i=1,timestep)
201    format(<timestep>(i3))
       write(*,*) IENS,GRIBID,KM,timestep,interval,nfhr
      
       if(GRIBID.eq.256) then
c        im=134                 !2008 Beijing Olympic Game domain
c        jm=101
         im=884                 !HiresWindow (USeast and USwest)
         jm=614
         jf=im*jm
         GRIBID=255
        else
         if(GRIBID.eq.257) then
         im=825                 !HiresWindow (AK)
         jm=603
         jf=im*jm
         GRIBID=255
        else
         if(GRIBID.eq.258) then
         im=223                 !HiresWindow (HI and PR)
         jm=170
         jf=im*jm
         GRIBID=255
        else
         if(GRIBID.eq.259) then
         im=1073                 !5km NDGD grid (now changed to 197 officially)
         jm=689
         jf=im*jm
         GRIBID=259
         dx=5.079*1000.
         dy=5.079*1000.
        else
         if(GRIBID.eq.132) then
         im=697                 !16km double resolution NA grid
         jm=553
         jf=im*jm
         GRIBID=132
         dx=16.0*1000.
         dy=16.0*1000.
       else
         call makgds(GRIBID, kgdss, gdss, lengds, ier)
         im=kgdss(2)
         jm=kgdss(3)
         jf=kgdss(2)*kgdss(3)
         dx=1.*kgdss(8)
         dy=1.*kgdss(9)


           end if
          end if
         end if
        end if
       end if
 
       write(*,*) 'im, jm, jf=', im, jm, jf
       loutput=timestep
       write(*,*) 'timestep, interval, loutput=', 
     +             timestep, interval, loutput

c  allocate raw data
       allocate(precip(jf,iens,loutput))             !save all itime's precip 
       allocate(mrk_ice(jf,iens,loutput))            !save all itime's ice mark
       allocate(mrk_frz(jf,iens,loutput))            !save all itime's freezing rain mark
       allocate(mrk_snow(jf,iens,loutput))           !save all itime's snow mark
       allocate(rawdata_mn(jf,iens,numvar,maxmlvl))
       allocate(rawdata_pr(jf,iens,numvar,maxplvl))
       if(.NOT.allocated(t4cooling)) then
        allocate(t4cooling(jf,iens,loutput,14))       !save all itime's cooling
       end if
       if(.NOT.allocated(t2m4cooling)) then
        allocate(t2m4cooling(jf,iens,loutput))        !save all itime's cooling
       end if
       if(.NOT.allocated(LWCfield)) then
        allocate(LWCfield(jf,iens,loutput))           !save all itime's fog LWC
       end if

       allocate (prcp(jf,loutput))
       allocate (m_frz(jf,loutput))
       allocate (m_ice(jf,loutput))
       allocate (m_snow(jf,loutput))
       allocate (t4fog(jf,14))
       allocate (t2m4fog(jf))


c allocate mean


       allocate(vrbl_mn(jf, numvar, maxmlvl))
       allocate(derv_mn(jf, nderiv, maxmlvl))

       allocate(prcpmax(jf,maxmlvl))
       allocate(prcpmin(jf,maxmlvl))
       allocate(snowmax(jf,maxmlvl))
       allocate(snowmin(jf,maxmlvl))
       allocate(frznmax(jf,maxmlvl))
       allocate(frznmin(jf,maxmlvl))


c spread

       allocate(vrbl_sp(jf, numvar, maxmlvl))
       allocate(derv_sp(jf, nderiv, maxmlvl))

c prob

       allocate(vrbl_pr(jf, numvar, maxplvl, maxtlvl))
       allocate(derv_pr(jf, nderiv, maxplvl, maxtlvl))

c mxp8
       allocate(mxp8(jf,nmxp,maxmlvl,8))

c others
       allocate(lb(jf))
       allocate(fhead(iens))   
       allocate(var(jf))
       allocate(apoint(iens)) 
       allocate(ptype_mn(jf,maxmlvl,4))
       allocate(ptype_pr(jf,maxmlvl,4))

       allocate(derv_dtra(jf,maxmlvl,10))
       allocate(Hsfc(jf))
       allocate(missing(iens)) 


       write(*,*) 'Array allocation done!'

c read file names' head
       write(*,*) iens, ' GRIB files are read:' 
       do i=1,IENS
        read(1,'(A)') oneline
        call ST_CLST( oneline, ' ', ' ', 5, substr, nsubstr, ier ) 
        call ST_RMBL( substr(1), fhead(i), lng, ier)  
        call ST_RMBL( substr(2), models, lng, ier)  
        write(*,*) fhead(i), models
       end do
c       read(1,*) ihh               !read in forecast hour to be processed

C J. Du note on 3/19/2010: use the first 4 characters of the last model's name 
c to name the final product output files (mean, spread and prob). So, the product
c files name needs to be adjusted when model name changes in production!
       eps=trim(models)
       

       close (1)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


c In these lines put here, we need the date and time of the forecast,
c the name of the file.  We need to open the file.  Then we can point to
c the correct location of the forecast in the forecast file.  If this
c is an ensemble, then we also need another loop for the forecast name
c (cntl, n1, p1, etc.).  That loop should be inside the date/time loop.
c
 
c Passing over date information
c
c      CALL W3TAGB('SREF_COM_GRIB',2002,0002,0074,'NP21')
C
c     read(10,namin,end=1000)
      read(5,namin,end=1000)
      imin=0

cc      t4cooling=0.
      t2m4cooling=0.
      LWCfield=0.
      prcp=0.
      m_frz=0.     
      m_ice=0.     
      m_snow=0.     
      t4fog=0.
      t2m4fog=0.

1000  continue
      write(*,*)'YR MON DAY CYCLE HH1 HH2 SYS:', 
     +     iyr,imon,idy,ihr,ihh1,ihh2,sys

       print*,'About to enter time loop'

C first loop: forecast times-------------------------------------------

        if(nfhr.lt.0) then       !non-cluster
c          n1=1
c          n2=timestep
          n1=ihh1
          n2=ihh2
        else                     !cluster case
          n1=nfhr
          n2=nfhr
        end if

cccc        DO 3000 i00=1,timestep                       !1,2,3,..... order
        DO 3000 i00=n1,n2                             !1,2,3,..... order
         itime=fnn(i00)                               !00,03,06,09,12...fcst hour
         print*,'Initializing variables'
         write(*,*) 'itime=',itime,' i00=',i00

c  Second loop: for ensmeble members:----------------------------------------
        DO 2000 irun=1,iens                       ! perturbation members

        if(itime.lt.100) then
         write(hr,'(i2.2)') itime    !itime is forecast fours 
        else
         write(hr,'(i3.3)') itime
        end if

        write(cyc,'(i2.2)') ihr       !ihr is cycle 
        write(fhr,'(i3.3)') itime     !fhr is forecast hour :  Changed to I3 for hrs >100 jtm
        write(*,*) 'cyc,fhr=', cyc,fhr

        fname=trim(fhead(irun)) // '.f' // trim(hr)

        iunit=10
        iout=50

        print*,'Opening ',fname
        call baopenr(iunit,fname,ierr)

c        print*,'itime=',itime, ' for member# ', irun

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c STEP (1): 
c Read various variables from the grib file, depending on the value
c of kpds(5) (the type of variable) and kpds(7) (the height of the variable.
c Certain derived quantities are at the end of the list.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       iseek=0
       call skgb(iunit,iseek,llgrib,llskip)
       DO while(llgrib.gt.0)  ! llgrib 

        call rdgb(iunit,llgrib,llskip,kpds,kgds,jff,lb,var)
        call skgb(iunit,iseek,llgrib,llskip)

       if (jff.eq.0) var=-999.0

       do nv = 1, numvar

         do lv = 1, Mlvl(nv)
           if(kpds(5).eq.k5(nv).and.kpds(6).eq.k6(nv).and.
     +                       kpds(7).eq.MeanLevel(nv,lv)) then
c             do igrid=1,jf
              if(kpds(5).eq.131.and.kpds(6).eq.101.and.
     +                      kpds(7).eq.12900) then
               rawdata_mn(:,irun,nv,lv) = var(:) - 273.15 !Jun Du: change unit from K to C for lifted index
              else
               rawdata_mn(:,irun,nv,lv) = var(:)
              end if
             
              if(kpds(5).eq.7.and.kpds(6).eq.1.and.itime.gt.0) then
               rawdata_mn(:,irun,nv,lv) = Hsfc(:)
              end if

              if(kpds(5).eq.20.and.kpds(6).eq.1) then   !visibility
c               if((irun.ge.4.and.irun.le.7).or.irun.ge.13) then
                if(irun.eq.13) then
                    rawdata_mn(:,irun,nv,lv) = 24135.0
                end if
              end if

c             if(kpds(5).eq.7.and.kpds(6).eq.215) then   !ceiling
c               if(irun.ge.4.and.irun.le.7) then
c                   rawdata_mn(:,irun,nv,lv) = 200001.0
c               end if
c             end if

c reprocess precip data based on different models and fcst hours when necessary
              if(kpds(5).eq.61.and.kpds(6).eq.1.and.itime.ge.1) then
                if(models.eq.'sref_eta') then
                  rawdata_mn(:,irun,nv,lv)=var(:)*1.0
                 else if (models.eq.'sref_rsm') then
                  rawdata_mn(:,irun,nv,lv)=var(:)*1.0
                 else if (models.eq.'sref_nmm') then
                  rawdata_mn(:,irun,nv,lv)=var(:)*1.0
                 else if (models.eq.'sref_em') then
                  rawdata_mn(:,irun,nv,lv)=var(:)*1.0
                 else if (models.eq.'sref_nmb') then
                  rawdata_mn(:,irun,nv,lv)=var(:)*1.0
                end if
              end if
c             end do

 
            if(kpds(5).eq.7.and.kpds(6).eq.1.and.itime.eq.0) then
              Hsfc(:) = var (:)
            end if
             
           end if
         end do

         do lv = 1, Plvl(nv)
           if(kpds(5).eq.k5(nv).and.kpds(6).eq.k6(nv).and.
     +                       kpds(7).eq.ProbLevel(nv,lv)) then
c             do igrid=1,jf
              if(kpds(5).eq.131.and.kpds(6).eq.101.and.
     +                      kpds(7).eq.12900) then
               rawdata_pr(:,irun,nv,lv) = var(:) - 273.15 !Jun Du: change unit from K to C for lifted index
              else if (kpds(5).eq.61.and.kpds(6).eq.1.and.
     +                 itime.ge.1) then
                if(models.eq.'sref_eta') then
                 rawdata_pr(:,irun,nv,lv)=var(:)*1.0
                else if (models.eq.'sref_rsm') then
                 rawdata_pr(:,irun,nv,lv)=var(:)*1.0
                else if (models.eq.'sref_nmm') then
                 rawdata_pr(:,irun,nv,lv)=var(:)*1.0
                else if (models.eq.'sref_em') then
                 rawdata_pr(:,irun,nv,lv)=var(:)*1.0
                else
                  rawdata_pr(:,irun,nv,lv) = var(:)
                end if
              else
               rawdata_pr(:,irun,nv,lv) = var(:)
              end if
c             end do

           end if
         end do

       end do 

ccc     store accumulated variable-related info:  cccccccccccccccccccc
           if (ihh1.ne.ihh2) Then
       if(kpds(5).eq.61.and.kpds(6).eq.1.and.kpds(7).eq.0) then
         ID_PRCP=index_table(k5,k6,61,1,maxvar)
         if(ID_PRCP.gt.0.and.itime.ge.1) then
          precip(:,irun,i00)=rawdata_mn(:,irun,ID_PRCP,1)
          if (sys.eq.111) print*,'Ensemble System is SREF'
          if (sys.eq.222) then
          print*,'Ensemble System is GEFS'
!jtm  if(itime.eq.06.or.itime.eq.12.or.itime.eq.18.or.itime.eq.24.or.
!jtm &   itime.eq.30.or.itime.eq.36.or.itime.eq.42.or.itime.eq.48.or.
!jtm &   itime.eq.54.or.itime.eq.60.or.itime.eq.66.or.itime.eq.72.or.
!jtm &   itime.eq.78.or.itime.eq.84)
      if(itime.gt.0.and.mod(itime,6).eq.0)
     &   precip(:,irun,i00)=precip(:,irun,i00)-precip(:,irun,i00-1) !Jun Du: to convert GEFS's 6hrly apcp to 3hrly
          endif
         end if 
       end if


       if(kpds(5).eq.141.and.kpds(6).eq.1.and.kpds(7).eq.0) then
         ID_FRZ=index_table(k5,k6,141,1,maxvar)
         if(ID_FRZ.gt.0.and.itime.ge.1) then
            mrk_frz(:,irun,i00)=rawdata_mn(:,irun,ID_FRZ,1)
         end if
       end if


       if(kpds(5).eq.142.and.kpds(6).eq.1.and.kpds(7).eq.0) then
         ID_ICE=index_table(k5,k6,142,1,maxvar)
         if(ID_ICE.gt.0.and.itime.ge.1) then
            mrk_ice(:,irun,i00)=rawdata_mn(:,irun,ID_ICE,1)
         end if
       end if

       if(kpds(5).eq.143.and.kpds(6).eq.1.and.kpds(7).eq.0) then
         ID_SNOW=index_table(k5,k6,143,1,maxvar)
         if(ID_SNOW.gt.0.and.itime.ge.1) then
            mrk_snow(:,irun,i00)=rawdata_mn(:,irun,ID_SNOW,1)
         end if
       end if

        end if
cccccc

      ENDDO       ! end of while loop for llgrib

      call baclose(iunit,ierr)


2000  continue    ! end of irun

c      call print_rawdata(rawdata_mn, rawdata_pr, jf, iens,
c     +     2051,2051)

 

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c STEP (2A):
c get mean and spread of direct variables
        write(*,*) 'get mean and spread ----------------------------'
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        do nv = 1, numvar
          if(trim(Msignal(nv)).eq.'M') then
           do lv = 1, Mlvl(nv)
            missing=1        !available
            if (k5(nv).eq.212.and.k6(nv).eq.200) then
             missing(4:7)=0  !missing

c            do i=1,iens
c             print*,'missing=',i,missing(i)
c            enddo

            end if
            if (k5(nv).eq.240.and.k6(nv).eq.200) then
             missing(4:7)=0  !missing
            end if

            do igrid=1,jf
              apoint = rawdata_mn(igrid,:,nv,lv)
             
              if(k5(nv).eq.20.and.k6(nv).eq.1) then                    ! Visibility: conditional mean
                call get_cond_mean (apoint,iens,24056.0,amean,aspread)

               if(igrid.eq.22863) then
                 write(*,*) 'Visibility: at ',igrid
                 write(*,*) '  Visbapoint=',apoint
                 write(*,*) '  amean=',amean, ' aspread=',aspread
               end if

              else if(k5(nv).eq.7.and.k6(nv).eq.215) then              ! Ceiling: conditional mean
                call get_cond_mean (apoint,iens,20000.0,amean,aspread)
               if(igrid.eq.1017) then
                 write(*,*) 'Ceiling at ',igrid
                 write(*,*) '  Ceilapoint=',apoint
                 write(*,*) '  amean=',amean, ' aspread=',aspread
               end if

              else if(k5(nv).eq.153.and.k6(nv).eq.100) then            ! fog LWC: liquid water content
                apoint = apoint * 1000.0                               !      kg/kg -> g/kg
                call get_cond_mean_lwc(apoint,iens,0.0,amean,aspread)   
                if(igrid.eq.2287) then
                 write(*,*) 'Fog LWC at:', lv, apoint,' mean=',amean
                end if

              else

c             if(k5(nv).eq.11.and.k6(nv).eq.105) then
c              do i=1,iens
c               print*,'missing=',i,missing(i)
c              enddo
c             end if
                call getmean(apoint,iens,amean,aspread)

              end if

              vrbl_mn(igrid,nv,lv)=amean
              vrbl_sp(igrid,nv,lv)=aspread

c          if(k5(nv).eq.11.and.k6(nv).eq.105) then
c            if(igrid.ge.10000.and.igrid.le.10050) then
c             write(*,'(i10,23f7.2)')igrid,(apoint(i),i=1,21),
c     +       vrbl_mn(igrid,nv,lv),vrbl_sp(igrid,nv,lv)
c            end if
c          end if


            end do
           end do
          end if
        end do


        write(*,*) 'finish direct variable mean and spread computation'
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c STEP (2B): 
c get prob for all direct variables at different levels for different thresholds
        write(*,*) 'get prob ----------------------------------------'
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        do nv = 1, numvar
          if(trim(Psignal(nv)).eq.'P') then
           do lv = 1, Plvl(nv)
            do lt = 1, Tlvl(nv)
              do igrid=1,jf
                apoint = rawdata_pr(igrid,:,nv,lv)
 
                 if(trim(op(nv)).ne.'-') then

                  thr1 = Thrs(nv,lt)
                  thr2 = 0.
                 if(k5(nv).eq.20.and.k6(nv).eq.1) then                  !20120520: no surface visibility from
                  call getprob_vis(apoint,iens,thr1,thr2,op(nv),aprob)  !nmmb's 4,5,6,7 members. so use getprob_vis
                 else
c                 if((k5(nv).eq.7.and.k6(nv).eq.215).or.                !ceiling
                  if((k5(nv).eq.212.and.k6(nv).eq.200).or.              !reflectivity
     +               (k5(nv).eq.240.and.k6(nv).eq.200)) then            !echo top
                  call getprob_cei(apoint,iens,thr1,thr2,op(nv),aprob)  
                  else
                  call getprob(apoint,iens,thr1,thr2,op(nv),aprob)
                  end if
                 end if
                  vrbl_pr(igrid,nv,lv,lt)=aprob
                 else
                  if(lt.lt.Tlvl(nv)) then
                    thr1 = Thrs(nv,lt)
                    thr2 = Thrs(nv,lt+1)

                    call getprob(apoint,iens,thr1,thr2,op(nv),aprob)
                       vrbl_pr(igrid,nv,lv,lt)=aprob
                   end if
                  end if

              end do

            end do
           end do
          end if
        end do


cc. Compute ceiling here since ARW doesn't output ceiling before it is updated to new SREF.
Cc  After the 2012 new SREF, this getceiling routine no need to be called.
c        do nv = 1, numvar    
c          if (k5(nv).eq.7 .and. k6(nv).eq.215) then                 
c               call getceiling(nv,rawdata_mn,jf,iens,
c     +             vrbl_mn, vrbl_sp, vrbl_pr)
c          end if
c        end do


cc  Compute cloud top above ground level
        do nv = 1, numvar
          if (k5(nv).eq.7 .and. k6(nv).eq.3) then
               call getcldtop(nv,rawdata_mn,jf,iens,
     +             vrbl_mn, vrbl_sp, vrbl_pr)
          end if
        end do       

        write(*,*) 'finish direct variable prob computation'
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c STEP (3):
c get mean/spread/prob for special derived variables
        write(*,*) 'get mean for derived variables -------------------'
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

          if (ihh1.ne.ihh2) then                 !non-grid#132 case
        do  nv = 1, nderiv                       !for all derived var (for new fog)
         write(*,*) 'dk5(nv), dk6(nv)=',dk5(nv), dk6(nv)
         if(dk5(nv).eq.146.and.dk6(nv).eq.105) then
          ID_TFOG=index_table_var(vname,k5,k6,11,100,'TFOG',numvar)
          if(ID_TFOG.gt.0) then
           do klv = 1,14
             t4cooling(:,:,i00,klv)=rawdata_mn(:,:,ID_TFOG,klv)
     +                              -273.15
           end do
          end if
          ID_T2M=index_table(k5,k6,11,105,maxvar)
          if(ID_T2M.gt.0) then
            t2m4cooling(:,:,i00) = rawdata_mn(:,:,ID_T2M,1)
     +                             -273.15
          endif
         end if
        end do
         end if

         do 100 nv = 1, nderiv                       !for all derived var

         write(*,*) 'derived var=',nv

cc%%%%%%% 1. To  see if there is thickness computation, if yes, do it

           if (dk5(nv).eq.7.and. dk6(nv).eq.101) then 

            call thickness (nv, rawdata_mn, jf, iens,
     +             derv_mn, derv_sp, derv_pr)
           end if
 

cc%%%%%%% 2. To see if there is precipitation type computation, if yes, do it

           if (dk5(nv).eq.140 .or. dk5(nv).eq.193 .or.
     +         dk5(nv).eq.194 .or. dk5(nv).eq.195) then

              call preciptype (nv, rawdata_mn, jf, iens,
     +             derv_mn, derv_pr, ptype_mn, ptype_pr)

           end if

cc%%%%%%% 3. To see if there is accumulated precipitation, if yes, do it

         if(dk5(nv).eq.61.and.dk6(nv).eq.1.and.itime.gt.0) then

           if (interval.eq.3.and.ihh1.eq.ihh2) then
            do irun = 1, iens 
             head=fhead(irun)
             write(*,*) 'prev precip irun=', irun
             if (i00.eq.2) then
               call get_prev_precip(i00,1,loutput,head,jf,
     +          prcp,m_frz,m_ice,m_snow)
                precip(:,irun,i00)=prcp(:,i00)
                mrk_frz(:,irun,i00) =  m_frz(:,i00)
                mrk_ice(:,irun,i00) =  m_ice(:,i00)
                mrk_snow(:,irun,i00)=  m_snow(:,i00)
             else if (i00.eq.3.or.i00.eq.4) then
               call get_prev_precip(i00,2,loutput,head,jf,
     +              prcp,m_frz,m_ice,m_snow)
              do m = 1,2
                np=i00+1-m
                precip(:,irun,np)=prcp(:,np)
                mrk_frz(:,irun,np) =  m_frz(:,np)
                mrk_ice(:,irun,np) =  m_ice(:,np)
                mrk_snow(:,irun,np)=  m_snow(:,np)
              end do
             else if (i00.ge.5.and.i00.lt.9) then
                call get_prev_precip(i00,4,loutput,head,jf,
     +              prcp,m_frz,m_ice,m_snow)
               do m = 1,4
                 np=i00+1-m
                 precip(:,irun,np)=prcp(:,np)
                 mrk_frz(:,irun,np) =  m_frz(:,np)
                 mrk_ice(:,irun,np) =  m_ice(:,np)
                 mrk_snow(:,irun,np)=  m_snow(:,np)
               end do
             else if (i00.ge.9) then
               call get_prev_precip(i00,8,loutput,head,jf,
     +              prcp,m_frz,m_ice,m_snow)
               do m = 1,8
                 np=i00+1-m
                 precip(:,irun,np)=prcp(:,np)
                 mrk_frz(:,irun,np) =  m_frz(:,np)
                 mrk_ice(:,irun,np) =  m_ice(:,np)
                 mrk_snow(:,irun,np)=  m_snow(:,np)
               end do
             end if
            end do
       
           end if

c          do irun=1,iens 
c           do ig =26359,26359
c           write(*,*) 'irun=',irun
c           write(*,*) ig, precip(ig,irun,:)
c           write(*,*) ig,mrk_frz(ig,irun,:)
c           write(*,*) ig,mrk_ice(ig,irun,:)
c           write(*,*) ig,mrk_snow(ig,irun,:)
c           end do
c          end do

          call apcp(nv,itime,i00,precip,jf,iens,interval,
     +           loutput, derv_mn,derv_sp,derv_pr,prcpmax,prcpmin)

         end if

cc%%%%%%% 4. To see if there is accumulated snow, if yes, do it

            if(dk5(nv).eq.79.and.dk6(nv).eq.1.and.itime.gt.0) then
             !note: snow acc must be put before rain acc in the variable table so that
             !precip,mrk_ice,mrk_snow can be obtained before "call snow here"                                                                                                                                     
              call snow(nv,itime,i00,rawdata_mn,precip,mrk_ice,mrk_snow,
     +         jf,iens,interval,loutput,derv_mn,derv_sp,derv_pr,
     +         snowmax,snowmin)

            end if

cc%%%%%%% 5. To see if there is wind speed computation, if yes, do it

            if(dk5(nv).eq.32) then

               call wind(nv,rawdata_mn,jf,iens,
     +             derv_mn, derv_sp, derv_pr)

            end if

cc%%%%%%% 6. To get DTRA variances, if yes, do it
            if(dk5(nv).eq.501) then
             
               call dtra(nv,rawdata_mn,jf,iens,
     +             derv_dtra)
             
            end if

cc%%%%%%%  7. To get fog, if yes, do it
c            if(dk5(nv).eq.146) then
c               call getfog(nv,rawdata_mn,jf,iens,derv_pr)
c            end if


cc%%%%%%% 8. To see if there is accumulated freezing rain, if yes, do it

            if(dk5(nv).eq.78.and.dk6(nv).eq.1.and.itime.gt.0) then

            write(*,*) 'call frzn'

              call frzn(nv,itime,i00,precip,mrk_ice,mrk_frz,jf,iens,
     +  interval,loutput, derv_mn,derv_sp,derv_pr,frznmax,frznmin)

            write(*,*) 'call frzn done'
            end if
         

        write(*,*) 'finish derived computation for ', dvname(nv)

cc%%%%%%%  9. Add absolute vorticity( for global ensemble )

c            if(dk5(nv).eq.41) then
c               call vorticity (nv,rawdata_mn,im,jm,jf,iens,
c     +             derv_mn, derv_sp, derv_pr)
c            end if
 
            

cc%%%%%%%  10. Add flight restriction probability computation

            if(dk5(nv).eq.20. and. dk6(nv).eq.2) then
              call flight_res(nv,rawdata_mn,jf,iens,derv_pr)
            end if

cc%%%%%%%  11. Add fireweather probability ...........................

            if(dk5(nv).eq.147. and. dk6(nv).eq.2) then
              call get_firewx(nv,rawdata_mn,jf,iens,derv_pr)
            end if

cc%%%%%%%  12. Add David Bright's CPTP probability ...........................

            if(dk5(nv).eq.301.and.dk6(nv).eq.200) then
              call get_cptp_severe (nv,itime,fhead,im,jm,km,jf,iens,
     +        interval,cyc,fhr,vrbl_pr,derv_pr)
            end if


cc%%%%%%%  13. Add LLWS
          if(dk5(nv).eq.136. and. dk6(nv).eq.107) then
           call llws(nv,rawdata_mn,jf,iens,derv_mn,derv_sp,derv_pr)
          end if


       if(itime.eq.42) then
        write(*,*) 'Check T before derived 14 '
        do  nr = 1, numvar
          do kk=10000,10005
           if(k5(nr).eq.11.and.k6(nr).eq.105) then
            write(*,'(i10,3i5,f7.2)')kk,k5(nr),k6(nr),nr,
     +         vrbl_mn(kk,nr,1)
           end if
           if(k5(nr).eq.11.and.k6(nr).eq.100) then
            write(*,'(i10,3i5,4f7.2)')kk,k5(nr),k6(nr),nr,
     +         (vrbl_mn(kk,nr,i),i=1,4)
           end if
          end do
         end do
        end if


c%%%%%%%  14. Asymptotic fog formulation is implemented here

      if(dk5(nv).eq.146.and.dk6(nv).eq.105.
     +                             and.itime.gt.0) then

         if(i00.gt.1) then
 
          if(ihh1.eq.ihh2) then                         ! grid#132 case
           do irun = 1,iens
            write(*,*) 'prev temp irun=', irun      
            head=fhead(irun)
            jtime=fnn(i00-1)
            write(fhr_1,'(i3.3)') jtime    !i3.3 for hrs > 100 jtm
 
             ID_RHFG=index_table_var(vname,k5,k6,52,100,'RHFG',numvar)  !if compute new fog algo, RHTF must be declared in deirect var
             write(*,*)'ID_RHFG=',ID_RHFG
            if (ID_RHFG.gt.0) then
             do k=1,14
              pfog(k)=MeanLevel(ID_RHFG, k)
              write(*,*) k, pfog(k)
             end do

             call get_prev_temp(fhr_1,head,jf,pfog,t2m4fog, t4fog)
             do k = 1,14
              t4cooling(:,irun,i00-1,k)=t4fog(:,k)-273.15
             end do
              t2m4cooling(:,irun,i00-1)=t2m4fog(:)-273.15

             call get_prev_temp(fhr,head,jf,pfog,t2m4fog, t4fog)
             do k = 1,14
              t4cooling(:,irun,i00,k)=t4fog(:,k)-273.15
             end do
              t2m4cooling(:,irun,i00)=t2m4fog(:)-273.15
            end if
          end do 
         end if

        call new_fog(nv,itime,i00,t4cooling,t2m4cooling,
     +     rawdata_mn,jf,im,jm,dx,dy,iens,interval,loutput,
     +     derv_mn,derv_sp,derv_pr,LWCfield)

        end if

       end if


cc%%%%%%%  15. To be added if necessary ...........................

100     continue 

        write(*,*)'>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>'
c        write(*,*) 'precip=', (precip(10,irun,i00),irun=1,iens)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c STEP 4. Compute max, min, 10,25,50,90% mean product
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         do 400 nv = 1, nmxp                       !for all mxp var
          if(trim(qMsignal(nv)).eq.'Q') then
          call get_mxp(nv,rawdata_mn,jf,iens,mxp8)
          end if
400      continue

c        call print_result(vrbl_mn,vrbl_sp,vrbl_pr,
c     +     derv_mn,derv_sp,derv_pr,ptype_mn,ptype_pr,derv_dtra,
c     +     prcpmax,prcpmin,snowmax,snowmin,frznmax,frznmin,
c     +     jf,iens,2051,2051)


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c STEP (5A): 
c Write results into grib files for direct variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if(itime.eq.42) then
        write(*,*) 'Check T before packing mean'
        do  nv = 1, numvar
          do kk=10000,10050
           if(k5(nv).eq.11.and.k6(nv).eq.105) then
            write(*,'(i10,3i5,f7.2)')kk,k5(nv),k6(nv),nv,
     +         vrbl_mn(kk,nv,1)
           end if
           if(k5(nv).eq.11.and.k6(nv).eq.100) then
            write(*,'(i10,3i5,4f7.2)')kk,k5(nv),k6(nv),nv,
     +         (vrbl_mn(kk,nv,i),i=1,4)
           end if
          end do
         end do      
        end if

        call pack(itime,eps,vrbl_mn,vrbl_sp,vrbl_pr,
     +            derv_mn,derv_sp,derv_pr,
     +            ptype_mn,ptype_pr,derv_dtra,
     +            prcpmax,prcpmin,snowmax,snowmin,
     +            frznmax,frznmin,
     +            jf,iens,iyr,imon,idy,ihr,
     +            gribid,lb,kgds,interval)

        call pack_mxp(itime,eps,mxp8,
     +    jf,iens,iyr,imon,idy,ihr,gribid,lb,kgds,interval)

3001      iunit=iunit+1

        write(*,*) 'Forecast f', itime, ' done!'

3000    continue   ! end of itime


c release all space:

       deallocate(precip)
       deallocate(rawdata_mn)
       deallocate(rawdata_pr)
       deallocate(vrbl_mn)
       deallocate(derv_mn)
       deallocate(prcpmax)
       deallocate(prcpmin)
       deallocate(snowmax)
       deallocate(snowmin)
       deallocate(frznmax)
       deallocate(frznmin)
       deallocate(vrbl_sp)
       deallocate(derv_sp)
       deallocate(vrbl_pr)
       deallocate(derv_pr)
       deallocate(lb)
       deallocate(fhead)
       deallocate(var)
       deallocate(apoint)
       deallocate(ptype_mn)
       deallocate(ptype_pr)
       deallocate(derv_dtra)
       deallocate(mrk_ice)
       deallocate(mrk_frz)
       deallocate(mrk_snow)
       deallocate(Hsfc) 
       deallocate(t4cooling)
       deallocate(t2m4cooling)
       deallocate(LWCfield)
       deallocate (prcp)
       deallocate (m_frz)
       deallocate (m_ice)
       deallocate (m_snow)
       deallocate (t4fog)
       deallocate (t2m4fog)
       deallocate (missing)

      stop
      end

