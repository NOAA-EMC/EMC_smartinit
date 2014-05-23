      program mm5toncll

C converts mm5 output to netcdf output readable by IVE

c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c
c   Model output header variables.
c
      integer   jyr(18),jmo(18),jdy(18),jhr(18)
      dimension sigf(200), plev(200), idumb(100), mif(30)
      real      mrf(10)
      logical   mlf(10)
      integer bhi(50,20), flag
      real bhr(20,20)
      character*80 bhic(50,20),bhrc(20,20)
      integer ndim
      real time
      integer start_index(4), end_index(4)
      character staggering*4, ordering*4,
     &    current_date*24, name*9, unitsr*25, description*46
c
      character dataform*8, argum(256)*256

co      integer   mifv1(1000,20)
co      real      mrfv1(1000,20)
co      character*80 mifc(1000,20), mrfc(1000,20)
co      character dataform*8, argum(16)*90
c
c   Get command line arguments.
c
      nargum=iargc()
      if (nargum.lt.2) then
         print*,'Usage:'
         print*,' mm5tonc output_file input_file'
         stop
      endif
      do i=1,nargum
         argum(i)='                                '//
     &            '                                '
         call getarg(i,argum(i))
      enddo
      nsets=nargum-1
      nsetsbeg=2
c
c   Read header record to get dimensions
c
      iudatin=21
      open (unit=iudatin,file=argum(nsetsbeg),form='unformatted',
     &   status='old')
c
      read(iudatin,err=140,end=180) flag
      if (flag.ne.0) then
         print*,'MM5 V3 data does not begin with a big header.'
         print*,'Stopping.'
         stop
      endif
      read(iudatin) bhi, bhr, bhic, bhrc

c      do ii = 1,50
c         do jj = 1,20
c            print *,ii,jj,bhi(ii,jj),bhic(ii,jj)
c            pause
c         end do
c      end do

c
      dataform='mm5v3   '
      iprog=bhi(1,1)
      miy = bhi(16,1)
      mjx = bhi(17,1)
      rewind (iudatin)
      print *,'iprog= ',iprog
      if (iprog.eq.1) then
         mkzh=1
      elseif (iprog.eq.2.or.iprog.eq.3) then
c
c      Number of levels needs to be obtained from the header for a
c      3D array--can't trust bhi(12,iprog) to be correct.
c
         read(iudatin) flag
         if (flag.ne.0) then
            print*,'MM5 V3 data does not begin with a big header.'
            print*,'Stopping.'
            stop
         endif
         read(iudatin) bhi, bhr, bhic, bhrc
 703     read(iudatin) flag
         if (flag.eq.1) then
            read (iudatin) ndim,start_index,end_index,time,
     &         staggering,ordering,current_date,name,
     &         unitsr,description
            if (name.ne.'U        ') then
               read(iudatin)
               goto 703
            else
               ntotlevels=end_index(3)-start_index(3)+1
               mkzh=ntotlevels-1   ! Don't include surface level in mkzh
            endif
         else
            print*,'Ran into a flag not =1 looking for U.'
            stop
         endif
         rewind (iudatin)
      elseif (iprog.eq.5.or.iprog.eq.11) then
         mkzh = bhi(12,iprog)
      else
         print*,'For MM5V3 data, can only read output from'
         print*,'Terrain, Regrid, Rawins/Little_r, Interp, or MM5.'
         stop
      endif
      if (iprog.ge.1.and.iprog.le.3) then
c
c      Need to determine if this is "expanded domain".
c      Look for 'TERRAIN' variable.
c
         read(iudatin) flag
         if (flag.ne.0) then
            print*,'MM5 V3 data does not begin with a big header.'
            print*,'Stopping.'
            stop
         endif
         read(iudatin) bhi, bhr, bhic, bhrc
 733     read(iudatin) flag
         if (flag.eq.1) then
            read (iudatin) ndim,start_index,end_index,time,
     &         staggering,ordering,current_date,name,
     &         unitsr,description
            if (name.ne.'TERRAIN  ') then
               read(iudatin)
               goto 733
            else
               if (end_index(1).ne.miy.or.end_index(2).ne.mjx) then
c
c               Looks like this is an expanded domain.
c
                  miy=end_index(1)-start_index(1)+1
                  mjx=end_index(2)-start_index(2)+1
                  if (bhi(8,1).ne.1.or.bhi(13,1).ne.1.or.
     &                miy.ne.bhi(9,1).or.mjx.ne.bhi(10,1)) then
                     print*,'Looks like an expanded domain but ',
     &                  'some things in headers are not consistent.'
                     print*,'bhi(8,1),bhi(13,1),bhi(9,1),bhi(10,1)='
                     print*,bhi(8,1),bhi(13,1),bhi(9,1),bhi(10,1)
                     print*,'miy,mjx (dervied from "TERRAIN" ',
     &                      'variable header)='
                     print*,miy,mjx
                     stop
                  endif
                  iexpanded=1
               endif
            endif
         else
            print*,'Ran into a flag not =1 looking for TERRAIN.'
            stop
         endif
         rewind (iudatin)
      endif
      goto 200
 140  continue
c
      print*,'The model data header is not a format'//
     &   ' that RIPDP recognizes.  Stopping.'
      stop
c
 180  print*,'Unexpected EOF reached when trying to read'
      print*,'model data header.  Stopping.'
      stop
c
 200  continue

      close (iudatin)
      call process(miy,mjx,mkzh,dataform,iexpanded,
     &   argum,nnl,ncn,nsets,nsetsbeg,mif,mrf,mlf,mifv1,mrfv1,
     &   mifc,mrfc,bhi,bhr,bhic,bhrc)
      stop
      end
c                                                                     c
c*********************************************************************c
c                                                                     c
      subroutine process(miy,mjx,mkzh,dataform,iexpanded,
     &   argum,nnl,ncn,nsets,nsetsbeg,mif,mrf,mlf,mifv1,mrfv1,
     &   mifc,mrfc,bhi,bhr,bhic,bhrc)

c
c   This subroutine does most of the "work".
c
c   miy, and mjx are dot-point dimensions, in the x and y directions
c      respectively, of the domain to be analyzed.
c   mkzh is number of 1/2-sigma levels in the domain.
c   miyi, mjxi, and mkzhi are the dimensions for ice variables,
c      which will all be "1" if there are no ice variables.
c   dataform tells what format the model data is.
c   argum carries the names of the model data files.
c   nsets is the number of files in the model dataset.
c   nsetsbeg is the element of argum that holds the first file name
c      of the model dataset.
c   mif, mrf, mlf, mifv1, mrfv1, mifc, and mrfc are the model system
c      header variables
c
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c
      include '/usr/local/netcdf/include/netcdf.inc'    

      character dataform*8, argum(256)*256
c
c   Other parameters (which shouldn't need to be changed
c      for most applications):
c
c   nvq, nvtq, and nvvq are the number of PV, temp., and u/v
c      partitions (set ipvdim to whatever ipv is in the data set).
c
c      parameter (ipvdim=0, nvq=1, nvtq=5, nvvq=1)
c
      dimension ter(miy,mjx),ter_tsf(miy,mjx),xmap(miy,mjx),
     &   cor(miy,mjx),xlus(miy,mjx),sno(miy,mjx),pstx(miy,mjx),
     &   rtc(miy,mjx),rte(miy,mjx),pstd(miy,mjx),tgk(miy,mjx),
     &   dmap(miy,mjx),sst(miy,mjx),pblh(miy,mjx),
     &   regime(miy,mjx),sshflux(miy,mjx),slhflux(miy,mjx),
     &   ust(miy,mjx),swdown(miy,mjx),lwdown(miy,mjx),
     &   soil1(miy,mjx),soil2(miy,mjx),soil3(miy,mjx),soil4(miy,mjx),
     &   soil5(miy,mjx),soil6(miy,mjx),uuu_sfan(miy,mjx),
     &   vvv_sfan(miy,mjx),tmk_sfan(miy,mjx),qvp_sfan(miy,mjx),
     &   slp(miy,mjx),sigh(mkzh),sigf(mkzh+1),
     &   dotcor(miy,mjx),tmk(miy,mjx,mkzh),uuu(miy,mjx,mkzh),
     &   ght(miy,mjx,mkzh),www(miy,mjx,mkzh+1),
     &   vvv(miy,mjx,mkzh),prs(miy,mjx,mkzh),prs_tsf(miy,mjx),
     &   qvp(miy,mjx,mkzh),qcw(miy,mjx,mkzh),qra(miy,mjx,mkzh),
     &   qci(miy,mjx,mkzh),qsn(miy,mjx,mkzh),
     &   qgr(miy,mjx,mkzh),rnci(miy,mjx,mkzh),
     &   tke(miy,mjx,mkzh),radtnd(miy,mjx,mkzh),
c     &   qqn(miy,1+ipvdim*(mjx-1),1+ipvdim*(mkzh-1),nvq),
c     &   tqn(miy,1+ipvdim*(mjx-1),1+ipvdim*(mkzh-1),nvtq),
c     &   uqn(miy,1+ipvdim*(mjx-1),1+ipvdim*(mkzh-1),nvvq),
c     &   vqn(miy,1+ipvdim*(mjx-1),1+ipvdim*(mkzh-1),nvvq),
     &   scr3(miy,mjx,mkzh),scr2(miy,mjx),
     &   rrbo(miy,mjx),clgo(miy,mjx),viso(miy,mjx),
     &   clgf(miy,mjx),visf(miy,mjx),rrc(miy,mjx),rre(miy,mjx),
     &   xmav(miy,mjx),dter(miy,mjx),rpc(miy,mjx),rpe(miy,mjx),
     &   t2(miy,mjx),q2(miy,mjx),u10(miy,mjx),v10(miy,mjx),
     &   swout(miy,mjx),lwout(miy,mjx),xlat(miy,mjx),xlon(miy,mjx)

      character pvdfname*8,varname*10,fname*90,cxtime*9,
     &   cxtimeavl(200)*9,runnam*80

c new vars for cdl
   
      integer nfld,ndim(100),ioflg(100),nbase(100),i1d,i2d,i3d
      integer cdfid,varid(100),idz(100),idcon(20),idtime,status
      integer i4s(4),i4l(4),i3s(3),i3l(3),i2s(2),i2l(2)
      real sgout(mkzh+1)

      character*10 inname,fldnm(100)
      character*6 inunits,units(100)
      character*1 ingrid,grid(100)

c
c   Model output header variables.
c
      integer   jyr(18),jmo(18),jdy(18),jhr(18),plev(100),
     &   idumb(100), mif(30)
      real      mrf(10)
      logical   mlf(10)
      integer bhi(50,20), flag
      real bhr(20,20)
      character*80 bhic(50,20),bhrc(20,20)
      integer ndimr
      real time
      integer start_index(4), end_index(4)
      character staggering*4, ordering*4,
     &   current_date*24, name*9, unitsr*25, description*46
      dimension prslvl(200)
c
c   RIP header variables

      dimension ihrip(32),rhrip(32),fullsigma(128),halfsigma(128)
      character chrip(64)*64,vardesc*64,plchun*24
c
c minfo variables  JFB 12/26/98
c
      character cupa(10)*10, bltyp(10)*10, mphys(10)*10, version*6
      data cupa /'No Cumulus','Anthes-Kuo',' Grell ','Ara-Schu',
     & 'Fritsch-Ch','Kain-Frsch','Betts-Mill',' ',' ',' '/
      data bltyp /'No frict','Bulk PBL','Blackadar','Burk-Thomp',
     & 'Eta PBL','MRF PBL',' ',' ',' ',' '/
      data mphys /' Dry','Stable','Warm rain','Simple ice','Reisner 1',
     & 'GSFC Graup','Reisner 2','Schultz',' ',' '/
c
c   Namelist variables
c
      parameter (maxptimes=500)
      dimension ptimes(maxptimes),iptimes(maxptimes),ptuse(maxptimes)
      character discard(maxptimes)*9,ptimeunits*1

c      namelist/userin/ ptimes,iptimes,ptimeunits,tacc,discard,
c     &   iexpandedout,ipv,npvsets,iskpd1,iskppvd1,iobsprc,
c     &   iobscnv,iftcnv

c      print*,'Welcome to your friendly RIPDP output file !'

      iobsprc = 0
      iobscnv = 0
      iftcnv = 0
c
c   Define some constants
c
      rgas=287.04  !J/K/kg
      grav=9.81           ! m/s**2
      sclht=rgas*256./grav   ! 256 K is avg. trop. temp. from USSA.
      eps=0.622
      ezero=6.112  ! hPa
      pvc=1.e6
      celkel=273.15
      eslcon1=17.67
      eslcon2=29.65
      ussalr=.0065      ! deg C per m
c
c   Define unit numbers
c
      iuinput=7     ! input unit# for namelist, color table,
c                        and plspec table
      iudata=21     ! input unit# for the regular data.
      iupv=41       ! input unit# for the pv data.
      iuobsprc=91   ! input unit# for the observed precip data
      iuobsclg=92   ! input unit# for the observed ceiling data
      iuobsvis=93   ! input unit# for the observed visibility data
      iuftclg=94    ! input unit# for the FT ceiling data
      iuftvis=95    ! input unit# for the FT visibility data

c
c   Read the namelist values.
c
c      ipv=0
c      npvsets=0
c      iskpd1=0
c      iskppvd1=0
c      iobsprc=0
c      iobscnv=0
c      iftcnv=0
c      if (iexpanded.eq.1) then
c         print*,'Input data is on an expanded domain.'
c         if (iexpandedout.eq.1) then
c           print*,'RIPDP will process the full (expanded) domain.'
c         else
c           print*,'RIPDP will output the standard (unexpanded) domain.'
c         endif
c      endif
c      tacch=tacc/3600.
c
c   Determine number of names in "discard" array.
c
co      do i=1,maxptimes
co         if (discard(i).eq.'         ') then
co            ndiscard=i-1
co            goto 492
co         endif
co      enddo
co      ndiscard=maxptimes
co 492  continue
c
c
      iendc=index(argum(1),' ')-1
      if (iendc.eq.-1) iendc=90
      iendf1=iendc+11
      nxtavl=0
c
c   Define iounit for reading conventional data.
c
      iudatin=iudata
      open (unit=iudatin,file=argum(nsetsbeg),
     &   form='unformatted',status='old')
c
c   Open the observations files
c
      if (iobsprc.eq.1) open (unit=iuobsprc,
     &   file='obsprc.dat',form='unformatted',status='old')
      if (iobscnv.eq.1) then
         open (unit=iuobsclg,file='obsclg.dat',form='unformatted',
     &      status='old')
         open (unit=iuobsvis,file='obsvis.dat',form='unformatted',
     &      status='old')
      endif
      if (iftcnv.eq.1) then
         open (unit=iuftclg,file='ftclg.dat',form='unformatted',
     &      status='old')
         open (unit=iuftvis,file='ftvis.dat',form='unformatted',
     &      status='old')
      endif
c
c   Initialize rpc and rpe to zero.
c
      call fillarray(rpc,miy*mjx,0.)
      call fillarray(rpe,miy*mjx,0.)
c
c   Initialize "max" time level. This feature causes RIPDP to
c      ignore standard data which is out of chronological order.
c
      xtimemax=-1.
c
c   LOOP THROUGH TIME LEVELS.
c
c   Read header record of data file.
c
c   Note the different time specifications:
c
c   mdateb: This refers to the integer hour nearest to the beginning of
c     the model run for model output, or nearest to the starting
c     time for model input data.  It is an 8-digit integer
c     specified as YYMMDDHH.
c   mhourb: This variable refers to the same time as
c     mdateb, but instead of the YYMMDDHH format, it is specified
c     as the number of hours since 00 UTC 1 January 1 AD.
c   rhourb: This is a real number specifying the difference (in hours)
c     between the exact start time (of the model forecast or the
c     model input dataset) and the nearest integer hour to the start
c     time (specified by mdateb or mhourb).  Currently, both mm4
c     and mm5 datasets specify the start time only as an mdate,
c     and therefore rhourb will always be 0.00.  However, rhourb
c     could, in principal, range from -0.50 to +0.50.
c   xtime: This is a real number referring to this particular
c     data time, and is specified as the exact number of hours since
c     mhourb+rhourb.
c   mdate: This refers to the integer hour nearest to this particular
c     data time.  It is an 8-digit integer specified as YYMMDDHH.
c   mhour: This also refers to the integer hour nearest to this
c     particular data time.  It is an integer specified as the
c     number of hours since 00 UTC 1 January 1 AD.
c   rhour: This is a real number specifying the difference (in hours)
c     between the exact time of this dataset (represented by xtime) and
c     the integer hour nearest to the time of this dataset (represented
c     by mdate or mhour).  It can range from -0.50 to +0.50.
c
      ifirstread=1
 240  if (dataform.eq.'mm5v3   ') then
 796     read(iudatin,end=254,err=250) flag
         print *,'fl',flag
         if (flag.eq.0) then ! big header
            read(iudatin) bhi, bhr, bhic, bhrc
            goto 796
         elseif (flag.eq.2) then
            goto 254
         endif
         read (iudatin) ndimr,start_index,end_index,time,staggering,
     &      ordering,current_date,name,unitsr,description
         backspace (iudatin)
         backspace (iudatin)
         iprog=bhi(1,1)
         print *,'iprog2= ',iprog
         if (iprog.ge.2) then
            mdateb=1000000*mod(bhi(5,iprog),100)+10000*bhi(6,iprog)+
     &         100*bhi(7,iprog)+bhi(8,iprog)
            rhourb=bhi(9,iprog)/60.+bhi(10,iprog)/3600.+
     &         bhi(11,iprog)/36000000.
            call mconvert(mdateb,mhourb,1,1940)
            read(current_date,'(1x,6(1x,i2),1x,i4)')
     &         iyr,imo,idy,ihr,imn,isc,itt
            mdate=1000000*iyr+10000*imo+100*idy+ihr
            rhour=imn/60.+isc/3600.+itt/36000000.
            call mconvert(mdate,mhour,1,1940)
            xtime=float(mhour-mhourb)+rhour-rhourb
            if (iprog.eq.11) then
               xtime2=time/60.
               if (abs(xtime-xtime2).gt..001) then
                  print*,'Seems to be an inconsistency between "time"'
                  print*,'and "current_date" in V3 header.'
                  print*,'current_date,time,xtime,xtime2='
                  print*,current_date,'  ',time,xtime,xtime2
                  stop
               endif
            endif
         elseif (iprog.eq.1) then ! Terrain data - date/time info irrelevant
            mdateb=651106   ! Mark Stoelinga's birthday
            call mconvert(mdateb,mhourb,1,1940)
            rhourb=0.
            mdate=mdateb
            mhour=mhourb
            rhour=0.
            xtime=0.
         endif
      else
         print*,'   Unrecognized dataform.'
         stop
      endif
c
      if (ifirstread.eq.1) then
         print*,'Data is recognized as ',dataform,','
         print*,'   from program number ',iprog,'.'
         print*
      endif
      secondspast=rhour*3600.
      if (iprog.eq.1) then
         print*,' ****  Reading terrain data.'
      elseif (iprog.eq.2.or.iprog.eq.3) then
         print*,' ****  Reading pressure-level analysis data at'
      elseif (iprog.eq.5) then
         print*,' ****  Reading sigma-level analysis data at'
      elseif (iprog.eq.6.or.iprog.eq.11) then
         print*,' ****  Reading sigma-level model output at'
         print*,'       forecast time=',xtime
      endif
      if (iprog.eq.1) goto 235
      if (secondspast.lt..2) then
         write(6,926) mdate
      else
         write(6,927) mdate,secondspast
      endif
 926  format('        (YYMMDDHH = ',i8.8,')')
 927  format('        (YYMMDDHH = ',i8.8,' plus ',f12.5,' seconds)')
 235  continue
c
      goto 252
  250 print*,'Error reading header record of output file.'
      stop
  252 continue
c
      goto 256
  254 close (iudatin)
      iudatin=iudatin+1
      if (iudatin.eq.20+nsets+1) then
         print*
         print*,'*******   No more datasets   *******'
         goto 1000
      endif
      open (unit=iudatin,file=argum(iudatin-iudata+nsetsbeg),
     &   form='unformatted',status='old')
      goto 240
  256 continue
c
      if (ifirstread.eq.1) then
c         ifirstread=0
c
c      If using iptimes, convert the mdates in the iptimes array to
c      xtimes in the ptimes array.  Also, determine nptimes.
c
         nptimes=0
         nptuse = 0
co         print *,'time= ',itime
         if (nptuse.eq.0) ptusemax=9e9
c
c      Get some flags that are necessary for reading the right variables.
c
         if (dataform.eq.'mm5v3   ') then
            if (iprog.eq.5.or.iprog.eq.11) then
               refslp=bhr(2,5)
               refslt=bhr(3,5)
               reflaps=bhr(4,5)
               if (bhr(5,5).gt.100..and.bhr(5,5).lt.400.) then
                  refstratt=bhr(5,5)
               else
                  refstratt=.1  ! just above absolute zero
               endif
               inhyd=1
               if (iprog.eq.11) then
                  iice=bhi(18,11)
c
c               Check for possible lack of ice variables (in spite of iice
c               being =1 in V3 header), due to inconsistency in iice and
c               IMPHYS.
c
                  imphys=bhi(3,13)
                  iicemphys=0
                  if (imphys.ge.5) iicemphys=1
                  if (iice.eq.1.and.iicemphys.eq.0) then
                     print*,'Inconsistency in V3 header: iice=1 ',
     &                      'but imphys=',imphys
                     print*,'Imphys is more trustworthy, so we''ll',
     &                      ' set iice to 0.'
                     iice=0
                  endif
               else
                  iice=0
               endif
c
c            Find sigh (those @#$*&!$% NCAR people put it at the very END
c            of the output for a time period).
c
 603           read(iudatin) flag
               if (flag.eq.1) then
                  read (iudatin) ndimr,start_index,end_index,time,
     &               staggering,ordering,current_date,name,
     &               unitsr,description
                  if (name.ne.'SIGMAH   ') then
                     read(iudatin)
                     goto 603
                  else
                     if (end_index(1).ne.mkzh) then
                        print*,'SIGMAH has more elements than expected.'
                        print*,'end_index(1),mkzh=',end_index(1),mkzh
                        stop
                     endif
                     read(iudatin)sigh
                  endif
               else
                  print*,'Ran into a flag not =1 looking for SIGMAH.'
                  stop
               endif
               rewind (iudatin)
 199           read(iudatin) flag
               if (flag.eq.0) then ! big header
                  read(iudatin) bhi, bhr, bhic, bhrc
                  goto 199
               elseif (flag.eq.2) then
                  print*,'Something is wrong.  Ripdp has rewound'
                  print*,'the data file after finding SIGMAH, and'
                  print*,'is looking for the first big header, but'
                  print*,'flag=2, meaning "end of this time period".'
                  stop
               endif
               backspace (iudatin)
c
c            While we're here, write out model info to the Jim Bresch-inspired
c            ".minfo" file
c
               if (iprog .eq. 11) then
                  fname=argum(1)(1:iendc)//'.minfo'
                  open (unit=58,file=fname,form='formatted',
     &                  status='unknown')
                  write(version,'(a2,a1,i1,a1,i1)') 'V3','.',bhi(3,11),
     &               '.',bhi(4,11)
c                 print*,'version = ',version
c                 itime = nint(mrfv1(309,6)*mrfv1(101,1)/mrfv1(1,1))
                  itime = nint(bhr(2,12)/float(bhi(20,1)))
                  write(58,1021) version, cupa(bhi(2,13)),
     &               bltyp(bhi(4,13)+1),mphys(bhi(3,13)),
     &               nint(bhr(1,1)*.001/float(bhi(20,1))),
     &               bhi(12,5),itime
 1021             format('Model info: ',a6,1x,3(a10,1x),i4,' km, ',i3,
     &               ' levels, ',i4,' sec')
               endif
            elseif (iprog.eq.2.or.iprog.eq.3) then
               inhyd=0
               iice=0
c
c            Define pseudo-sigma levels.  First get pressure levels.
c
 713           read(iudatin) flag
               if (flag.eq.1) then
                  read (iudatin) ndimr,start_index,end_index,time,
     &               staggering,ordering,current_date,name,
     &               unitsr,description
                  if (name.ne.'PRESSURE ') then
                     read(iudatin)
                     goto 713
                  else
                     if (end_index(1).ne.mkzh+1) then
                        print*,'PRESSURE has more elements than',
     &                         ' expected.'
                        print*,'end_index(1),mkzh=',end_index(1),mkzh
                        stop
                     endif
c
c                  Read in pressure levels.  Reverse the order, so that
c                     prslvl(1) is the top (lowest prs).
c
                     read(iudatin)(prslvl(k),k=mkzh+1,1,-1)
c                     print*,'Surface given as p=',.01*prslvl(mkzh+1),
c     &                  ' hPa.'
                     do k=1,mkzh
                        prslvl(k)=.01*prslvl(k)  ! Pa to hPa
                     enddo
                  endif
               else
                  print*,'Ran into a flag not =1 looking for PRESSURE.'
                  stop
               endif
               rewind (iudatin)
 299           read(iudatin) flag
               if (flag.eq.0) then ! big header
                  read(iudatin) bhi, bhr, bhic, bhrc
                  goto 299
               elseif (flag.eq.2) then
                  print*,'Something is wrong.  Ripdp has rewound'
                  print*,'the data file after finding PRESSURE, and'
                  print*,'is looking for the first big header, but'
                  print*,'flag=2, meaning "end of this time period".'
                  stop
               endif
               backspace (iudatin)
c
c            Now define half sigma levels
c
c               dpavg=(prslvl(mkzh)-prslvl(1))/(mkzh-1)
c               ptop=max(prslvl(1)-.5*dpavg,10.)  ! min of 10 hPa for ptop
c               pbot=prslvl(mkzh)+.5*dpavg
               ptop=prslvl(1)-1.  ! 1 hPa above highest half sigma level
               pbot=prslvl(mkzh)+1.  ! 1 hPa below lowest half sigma level
               pstarconst=pbot-ptop
               do k=1,mkzh
                  sigh(k)=(prslvl(k)-ptop)/pstarconst
               enddo
            elseif (iprog.eq.1) then ! Terrain data
               inhyd=0
               iice=0
c
c            Define pseudo-sigma levels.
c
               ptop=50.
               pbot=1000.
               pstarconst=pbot-ptop
               sigh(1)=.5
            endif
         else
            refslp=1.e5  ! this is in Pascals
            refslt=mrf(9)
            reflaps=mrf(10)
            refstratt=.1  ! just above absolute zero
         endif
c
      endif
c
c   Get important info from headers.
c
      if (dataform.eq.'mm5v3   ') then
         sigf(1)=0.
         if (iprog.eq.1) then
            sigf(2)=1.0
         elseif (iprog.eq.2.or.iprog.eq.3) then
            sigf(mkzh+1)=1.0
c
c         Note!! In the following, the full sigma levels are defined as
c         the midpoint between the two surrounding half sigma levels.
c         This is different from the standard definition, in which the
c         half sigma levels are defined as the midpoint between the two
c         surrounding full levels.  It must be done this was in order to
c         guarantee that pseudo-sigma levels can be defined so that half
c         sigma levels are exactly coincident with the pressure levels in
c         the pressure level output.  THEREFORE, NO CODE SHOULD BE WRITTEN
c         IN RIPDP OR RIP THAT ASSUMES THE STANDARD RELATIONSHIP BETWEEN
c         HALF AND FULL SIGMA LEVELS.
c
            do k=2,mkzh
               sigf(k)=.5*(sigh(k-1)+sigh(k))
            enddo
         elseif (iprog.eq.5.or.iprog.eq.11) then
            do k=1,mkzh
               sigf(k+1)=2.*sigh(k)-sigf(k)
            enddo
         endif
         miycors=bhi(5,1)
         mjxcors=bhi(6,1)
         if (iexpanded.eq.1) then
            miycors=miy
            mjxcors=mjx
            ioffexp=bhi(11,1)
            joffexp=bhi(12,1)
         endif
         dskmc=.001*bhr(1,1)
         xlatc=bhr(2,1)
         xlonc=bhr(3,1)
         nproj=bhi(7,1)
         if (nproj.gt.3.or.nproj.lt.1) then
            print*,'   Map proj. #',nproj,' is not recognized.'
            stop
         endif
         if (bhic(23,1)(1:5).eq.'OLD  '.and.bhi(23,1).eq.7) then
            ilandset=1
         elseif (bhic(23,1)(1:5).eq.'USGS '.and.bhi(23,1).eq.16) then
            ilandset=2
         elseif (bhic(23,1)(1:5).eq.'SiB  '.and.bhi(23,1).eq.15) then
            ilandset=3
         else
            print*,'RIPDP does not recognize land use data set'
            print*,'specified in header.  bhi(23,1)=',bhi(23,1)
            print*,'bhic(23,1)=',bhic(23,1)
            stop
         endif
         truelat1=bhr(5,1)
         truelat2=bhr(6,1)
         if (abs(bhr(7,1)).ne.90.) then
            print*,'Rip is only designed to deal with map backgrounds'
            print*,'that have pole=90 deg. or -90 deg.'
            stop
         endif
         if (iprog.eq.5.or.iprog.eq.11) ptop=.01*bhr(2,2) ! want it in hPa
         yicorn=bhr(10,1)
         xjcorn=bhr(11,1)
         dskm=.001*bhr(9,1)
      endif
c
      dsc=dskmc*1000.
      ds=dskm*1000.
      refrat=dsc/ds

c   Look for matching time in observations data files
c
c      call getobs(iobsprc,iobsprcfnd,mdate,iuobsprc)
c      call getobs(iobscnv,iobsclgfnd,mdate,iuobsclg)
c      call getobs(iobscnv,iobsvisfnd,mdate,iuobsvis)
c      call getobs(iftcnv,iftclgfnd,mdate,iuftclg)
c      call getobs(iftcnv,iftvisfnd,mdate,iuftvis)

c
c   Set up nonhydrostatic stuff.
c
      if (inhyd.eq.1) then
         if (refslp.lt.800e2.or.refslp.gt.1200e2
     &       .or.refslt.lt.240..or.refslt.gt.360.
     &       .or.reflaps.lt.-5..or.reflaps.gt.200.) then
            print*,'   Refslp, Refslt and/or Reflaps seem funny.'
            print*,'   Using "reasonable" values:'
            print*,'   Refslp=1000e2, Refslt=290., Reflaps=50.'
            refslp=1000e2
            refslt=290.
            reflaps=50.
            refstratt=.1  ! just above absolute zero
         endif
      else
         refslp=1000e2
         refslt=290.
         reflaps=50.
         refstratt=.1  ! just above absolute zero
      endif

c
c   Set all "gotit" flags to 0, except for arrays that have been
c
      igotituuu=0
      igotituuu_sfan=0   ! "sfan" means "surface analysis"
      igotitvvv=0
      igotitvvv_sfan=0
      igotittmk=0
      igotittmk_sfan=0
      igotitqvp=0
      igotitrh=0
      igotitrh_sfan=0
      igotitqcw=0
      igotitqra=0
      igotitqci=0
      igotitqsn=0
      igotitqgr=0
      igotitrnci=0
      igotittke=0
      igotitradtnd=0
      igotitwww=0
      igotitprs=0
      igotitght=0
      igotitslp=0
      igotitpstx=0
      igotittgk=0
      igotitsst=0
      igotitrtc=0
      igotitrte=0
      igotitter=0
      igotitter_tsf=0   ! "tsf" means "at true surface"
      igotitxmap=0
      igotitdmap=0
      igotitdotcor=0
      igotitxlus=0
      igotitsno=0
      igotitpblh=0
      igotitregime=0
      igotitsshflux=0
      igotitslhflux=0
      igotitxmav=0     ! jfb
      igotitust=0
      igotitswdown=0
      igotitlwdown=0
      igotitsoil1=0
      igotitsoil2=0
      igotitsoil3=0
      igotitsoil4=0
      igotitsoil5=0
      igotitsoil6=0
      igotitxlat=0
      igotitxlon=0
      igotitswout=0
      igotitlwout=0
      igotitt2=0
      igotitq2=0
      igotitu10=0
      igotitv10=0
      igotitrtmp=0

      igotitrmol=0
      igotithlfx=0
      igotitxmfx=0
      igotitymfx=0
      igotitrnfx=0
      igotitswh=0
      igotitcxwave=0
      igotitcywave=0
      igotitueff=0
      igotitveff=0
      igotitznt=0
      igotithfx=0

c   If first time through
c   Set all "iout" flags to 1, to write data out to netcdf file.
c   read in from file.
c
      if (ifirstread.eq.1) then
         ifirstread = 0
         nfld = 1
         isav = 1
         i2d = 0
         i3d = 0
         itime = 0
         do i = 1,100
            varid(i) = -999
         end do
         open (11,file='/home/models/mm5tonc/IVE_v3.in'
     +           ,status='old')
 111  read (11,112,end=113) inflg,inname,indim,inbase,inunits,ingrid
 112     format(i1,1x,a10,i2,i2,1x,a6,1x,a1)
         ioflg(nfld) = inflg
         fldnm(nfld) = inname
         ndim(nfld) = indim
         nbase(nfld) = inbase
         units(nfld) = inunits
         grid(nfld) = ingrid
         if (indim.eq.2) i2d = i2d + 1
         if (indim.eq.3) i3d = i3d + 1
         nfld = nfld + 1
         print *,nfld-1, inflg, inname, indim, inbase, inunits, ingrid
c         pause
         goto 111

 113     close(11)
         nfld = nfld - 1

         i1d = 2
         numcom = 4
         runnam=argum(1)

         x0 = dskmc*(xjcorn-1.)
         y0 = dskmc*(yicorn-1.)

         cc1=rgas/grav*(-.5)*reflaps
         cc2=rgas/grav*(reflaps*log(.01*refslp)-refslt)
         cc3=rgas/grav*(refslt-.5*reflaps*log(.01*refslp))*
     >        log(.01*refslp)

         alnpref=log(sigh(mkzh)*((refslp/100.)-(ptop))+(ptop))
         z0=cc1*alnpref*alnpref+cc2*alnpref+cc3         

         pbot1 = 1000. - ptop
         alnpref=log(sigh(1)*pbot1+ptop)
         zmax = cc1*alnpref*alnpref+cc2*alnpref+cc3
         zmax = real(int((zmax+500.)/1000.)) * 1000.
c 
c  if first time through call subroutine to create cdl  
c  Then ncgen to create shell netcdf file
c
         call create_cdl(mjx,miy,mkzh,nfld,i1d,i2d,i3d,cdfid,varid,
     &       idz,idcon,idtime,numcon,twod,runnam,nlen,ioflg,
     &       fldnm,ndim,nbase,units,grid,x0,y0,z0,dskm,zmax)

c
c  write constants and 1d vars to netcdf file
c
         print *,"creating initial netcdf file"
         status = NF_PUT_VAR1_REAL(cdfid,idtime,1,0.)
         if (status.ne.NF_NOERR) call handle_err(status)

         status = NF_PUT_VAR1_REAL(cdfid,idcon(1),1,ptop*100.)
         if (status.ne.NF_NOERR) call handle_err(status)
         status = NF_PUT_VAR1_REAL(cdfid,idcon(2),1,refslp)
         if (status.ne.NF_NOERR) call handle_err(status)
         status = NF_PUT_VAR1_REAL(cdfid,idcon(3),1,refslt)
         if (status.ne.NF_NOERR) call handle_err(status)
         status = NF_PUT_VAR1_REAL(cdfid,idcon(4),1,reflaps)
         if (status.ne.NF_NOERR) call handle_err(status)

         do k = 1,mkzh+1
            sgout(k) = sigf(mkzh+2-k)
         end do
         status = NF_PUT_VARA_REAL(cdfid,idz(1),1,mkzh+1,sgout)
         if (status.ne.NF_NOERR) call handle_err(status)
         do k = 1,mkzh
            sgout(k) = sigh(mkzh+1-k)
         end do
         status = NF_PUT_VARA_REAL(cdfid,idz(2),1,mkzh,sgout)
         if (status.ne.NF_NOERR) call handle_err(status)

      end if

      if (itime.gt.0) then
         rtime = real(nint(xtime*10000000.))/10000000
         status = NF_PUT_VAR1_REAL(cdfid,idtime,itime+1,rtime)
         if (status.ne.NF_NOERR) call handle_err(status)
      end if

c
c   Initialize pressure array to zero
c
      do k=1,mkzh
      do j=1,mjx
      do i=1,miy
         prs(i,j,k)=0.
      enddo
      enddo
      enddo

      do k=1,128
         fullsigma(k)=9e9
         halfsigma(k)=9e9
      enddo
      fullsigma(mkzh+1)=sigf(mkzh+1)
      do k=1,mkzh
         fullsigma(k)=sigf(k)
         halfsigma(k)=sigh(k)
      enddo
c
c   Read in variables.
c
c      if (dataform.eq.'mm5v3   ') then
c
 421  read(iudatin) flag
      if (flag.eq.1) then
         read (iudatin) ndimr,start_index,end_index,time,
     &      staggering,ordering,current_date,name,
     &      unitsr,description
c         print*
c         print*,'ndim=',ndim
c         print*,'start_index=',start_index
c         print*,'end_index=',end_index
c         print*,'time=',time
c         print*,'staggering=',staggering
c         print*,'ordering=',ordering
c         print*,'current_date=',current_date
c         print*,'name=',name
c         print*,'units=',units
c         print*,'description=',description

c         if (name.eq.'PSEALVLD '.or.
c     &           name.eq.'LATITDOT '.or.
c     &           name.eq.'LONGIDOT '.or.
c     &           name.eq.'LATITCRS '.or.
c     &           name.eq.'LONGICRS '.or.
c     &           name.eq.'RES TEMP '.or.
c     &           name.eq.'RES TMP  '.or.
c     &           name.eq.'HSFC     ') then
c            read(iudatin)
c            print*,'   Discarding variable ',name
c            print*,'   because RIP does not need it.'
c            idiscard=1
         if (name.eq.'U        ') then
            if (iprog.eq.2.or.iprog.eq.3) then
               read(iudatin) uuu_sfan,
     &            (((uuu(i,j,k),i=1,miy),j=1,mjx),k=mkzh,1,-1)
               igotituuu_sfan=1
               print *,'read uuu_sfan'
            else
               read(iudatin) uuu
               print *,'read uuu'
            endif
            igotituuu=1
         elseif (name.eq.'V        ') then
            if (iprog.eq.2.or.iprog.eq.3) then
               read(iudatin) vvv_sfan,
     &            (((vvv(i,j,k),i=1,miy),j=1,mjx),k=mkzh,1,-1)
               igotitvvv_sfan=1
               print *,'read vvv_sfan'
            else
               read(iudatin) vvv
               print *,'read vvv'
            endif
            igotitvvv=1
         elseif (name.eq.'T        ') then
            if (iprog.eq.2.or.iprog.eq.3) then
               read(iudatin) tmk_sfan,
     &            (((tmk(i,j,k),i=1,miy),j=1,mjx),k=mkzh,1,-1)
               igotittmk_sfan=1
               print *,'read tmk_sfan'
            else
               read(iudatin) tmk
               print *,'read tmk'
            endif
            igotittmk=1
         elseif (name.eq.'Q        ') then
            if (iprog.ne.5.and.iprog.ne.11) then
               print*,'RIP only expects qvp in model input or output.'
               print*,'Can''t handle it here.  Stopping.'
               stop
            endif
            read(iudatin) qvp
            print *,'read qvp'
            igotitqvp=1
         elseif (name.eq.'RH       ') then
            if (iprog.ne.2.and.iprog.ne.3) then
               print*,'RIP only expects RH in prs-level data.'
               print*,'Can''t handle it here.  Stopping.'
               stop
            endif
            read(iudatin) qvp_sfan,
     &            (((qvp(i,j,k),i=1,miy),j=1,mjx),k=mkzh,1,-1)
            print *,'read rh, or qvp_sfan'
            igotitrh_sfan=1
            igotitrh=1
         elseif (name.eq.'CLW      ') then
            if (iprog.ne.5.and.iprog.ne.11) then
               print*,'RIP only expects qcw in model input or output.'
               print*,'Can''t handle it here.  Stopping.'
               stop
            endif
            read(iudatin) qcw
            print *,'read qcw'
            igotitqcw=1
         elseif (name.eq.'RNW      ') then
            if (iprog.ne.5.and.iprog.ne.11) then
               print*,'RIP only expects qra in model input or output.'
               print*,'Can''t handle it here.  Stopping.'
               stop
            endif
            read(iudatin) qra
            print *,'read qra'
            igotitqra=1
         elseif (name.eq.'ICE      ') then
            if (iprog.ne.5.and.iprog.ne.11) then
               print*,'RIP only expects qci in model input or output.'
               print*,'Can''t handle it here.  Stopping.'
               stop
            endif
            read(iudatin) qci
            print *,'read qci'
            igotitqci=1
         elseif (name.eq.'SNOW     ') then
            if (iprog.ne.5.and.iprog.ne.11) then
               print*,'RIP only expects qsn in model input or output.'
               print*,'Can''t handle it here.  Stopping.'
               stop
            endif
            read(iudatin) qsn
            print *,'read qsn'
            igotitqsn=1
         elseif (name.eq.'GRAUPEL  ') then
            if (iprog.ne.5.and.iprog.ne.11) then
               print*,'RIP only expects qgr in model input or output.'
               print*,'Can''t handle it here.  Stopping.'
               stop
            endif
            read(iudatin) qgr
            print *,'read qgr'
            igotitqgr=1
         elseif (name.eq.'NCI      ') then
            if (iprog.ne.5.and.iprog.ne.11) then
               print*,'RIP only expects rnci in model input or output.'
               print*,'Can''t handle it here.  Stopping.'
               stop
            endif
            read(iudatin) rnci
            print *,'read rnci'
            igotitrnci=1
         elseif (name.eq.'TKE      ') then
            if (iprog.ne.5.and.iprog.ne.11) then
               print*,'RIP only expects tke in model input or output.'
               print*,'Can''t handle it here.  Stopping.'
               stop
            endif
            read(iudatin) tke
            print *,'read tke'
            igotittke=1
         elseif (name.eq.'RAD TEND ') then
            if (iprog.ne.5.and.iprog.ne.11) then
              print*,'RIP only expects radtnd in model input or output.'
               print*,'Can''t handle it here.  Stopping.'
               stop
            endif
            read(iudatin) radtnd
            print *,'read radtnd'
            igotitradtnd=1
         elseif (name.eq.'W        ') then
            if (iprog.ne.5.and.iprog.ne.11) then
               print*,'RIP only expects www in model input or output.'
               print*,'Can''t handle it here.  Stopping.'
               stop
            endif
            read(iudatin) www
            print *,'read www'
            igotitwww=1
         elseif (name.eq.'PP       ') then
            if (iprog.ne.5.and.iprog.ne.11) then
               print*,
     &          'RIP only expects prs pert. in model input or output.'
               print*,'Can''t handle it here.  Stopping.'
               stop
            endif
            read(iudatin) prs
            print *,'read prs'
            igotitprs=1
         elseif (name.eq.'H        ') then
            if (iprog.ne.2.and.iprog.ne.3) then
               print*,'RIP only expects geop. hgt. in prs-level data.'
               print*,'Can''t handle it here.  Stopping.'
               stop
            endif
            read(iudatin) scr2,    ! surface ght is same as ter
     &            (((ght(i,j,k),i=1,miy),j=1,mjx),k=mkzh,1,-1)
            print *,'read ght'
            igotitght=1
         elseif (name.eq.'PSTARCRS ') then
            read(iudatin) pstx
            print *,'read pstx'
            igotitpstx=1
         elseif (name.eq.'PSEALVLC ') then
            read(iudatin) slp
            print *,'read slp'
            igotitslp=1
         elseif (name.eq.'GROUND T ') then
            read(iudatin) tgk
            print *,'read tgk'
            igotittgk=1
         elseif (name.eq.'TSEASFC  ') then
            read(iudatin) sst
            print *,'read sst'
            igotitsst=1
         elseif (name.eq.'RAIN CON ') then
            read(iudatin) rtc
            print *,'read rtc'
            igotitrtc=1
         elseif (name.eq.'RAIN NON ') then
            read(iudatin) rte
            print *,'read rte'
            igotitrte=1
         elseif (name.eq.'TERRAIN  ') then
            if (iprog.eq.2.or.iprog.eq.3) then
               read(iudatin) ter_tsf
               igotitter_tsf=1
            else
               read(iudatin) ter
               print *,'read ter'
               igotitter=1
               do j=1,mjx
               do i=1,miy
                  iph=min(i,miy-1)
                  jph=min(j,mjx-1)
                  imh=max(i-1,1)
                  jmh=max(j-1,1)
                  dter(i,j)=.25*(ter(iph,jph)+ter(imh,jph)+
     &                           ter(iph,jmh)+ter(imh,jmh))
c                  print *,i,j,dter(i,j)
c                  pause
               enddo
               enddo
               print *,'clac dter'
            endif
         elseif (name.eq.'MAPFACCR ') then
            read(iudatin) xmap
            print *,'read xmap'
            igotitxmap=1
         elseif (name.eq.'MAPFACDT '.or.name.eq.'MAPFADOT ') then
            read(iudatin) dmap
            print *,'read dmap'
            igotitdmap=1
         elseif (name.eq.'CORIOLIS ') then
            read(iudatin) dotcor
            print *,'read dotcor'
            igotitdotcor=1
            do j=1,mjx-1
            do i=1,miy-1
               cor(i,j)=.25*(dotcor(i,j)+dotcor(i+1,j)+
     &                     dotcor(i,j+1)+dotcor(i+1,j+1))
            enddo
            enddo
            print *,'calc xcor'
         elseif (name.eq.'RES TEMP ') then
            read(iudatin) rtmp
            print *,'read rtmp'
            igotitrtmp=1
         elseif (name.eq.'LATITCRS ') then
            read(iudatin) xlat
            print *,'read xlat'
            igotitxlat=1
         elseif (name.eq.'LONGICRS ') then
            read(iudatin) xlon
            print *,'read xlon'
            igotitxlon=1
         elseif (name.eq.'LAND USE ') then
            read(iudatin) xlus
            print *,'read xlus'
            igotitxlus=1
         elseif (name.eq.'SNOWCOVR ') then
            read(iudatin) sno
            print *,'read sno'
            igotitsno=1
         elseif (name.eq.'PBL HGT  ') then
            read(iudatin) pblh
            print *,'read pblh'
            igotitpblh=1
         elseif (name.eq.'REGIME   ') then
            read(iudatin) regime
            print *,'read regime'
            igotitregime=1
         elseif (name.eq.'SHFLUX   ') then
            read(iudatin) sshflux
            print *,'read sshflux'
            igotitsshflux=1
         elseif (name.eq.'LHFLUX   ') then
            read(iudatin) slhflux
            print *,'read slhflux'
            igotitslhflux=1
         elseif (name.eq.'MAVAIL   ') then
            read(iudatin) xmav
            print *,'read xmav'
            igotitxmav=1
         elseif (name.eq.'UST      ') then
            read(iudatin) ust
            print *,'read ust'
            igotitust=1
         elseif (name.eq.'SWDOWN   ') then
            read(iudatin) swdown
            print *,'read swdown'
            igotitswdown=1
         elseif (name.eq.'LWDOWN   ') then
            read(iudatin) lwdown
            print *,'read lwdown'
            igotitlwdown=1
         elseif (name.eq.'SWOUT   ') then
            read(iudatin) swout
            print *,'read swout'
            igotitswout=1
         elseif (name.eq.'LWOUT   ') then
            read(iudatin) lwout
            print *,'read lwout'
            igotitlwout=1
         elseif (name.eq.'SOIL T 1 ') then
            read(iudatin) soil1
            print *,'read soil1'
            igotitsoil1=1
         elseif (name.eq.'SOIL T 2 ') then
            read(iudatin) soil2
            print *,'read soil2'
            igotitsoil2=1
         elseif (name.eq.'SOIL T 3 ') then
            read(iudatin) soil3
            print *,'read soil3'
            igotitsoil3=1
         elseif (name.eq.'SOIL T 4 ') then
            read(iudatin) soil4
            print *,'read soil4'
            igotitsoil4=1
         elseif (name.eq.'SOIL T 5 ') then
            read(iudatin) soil5
            print *,'read soil5'
            igotitsoil5=1
         elseif (name.eq.'SOIL T 6 ') then
            read(iudatin) soil6
            print *,'read soil6'
            igotitsoil6=1
         elseif (name.eq.'T2       ') then
            read(iudatin) t2
            print *,'read t2'
            igotitt2=1
         elseif (name.eq.'Q2       ') then
            read(iudatin) q2
            print *,'read q2'
            igotitq2=1
         elseif (name.eq.'U10      ') then
            read(iudatin) u10
            print *,'read u10'
            igotitu10=1
         elseif (name.eq.'V10      ') then
            read(iudatin) v10
            print *,'read v10'
            igotitv10=1
         elseif (ordering.eq.'YXS '.or.ordering.eq.'YXW ') then
            print *,'unknown 3d field'
c
c         unknown 3d field
c
c         Note: V3 header: name*9, units*25, description*46
c              RIP header: varname*10,vardesc*64, plchun*24
c
            if (iprog.eq.2.or.iprog.eq.3) then
               read(iudatin) scr2,
     &            (((scr3(i,j,k),i=1,miy),j=1,mjx),k=mkzh,1,-1)
            else
               read(iudatin) scr3
            endif
            varname=name
            inname="0"
            do ic=10,1,-1
               if (inname.eq."0") then
                  if (varname(ic:ic).ne.' ') inname="1"
               else
                  if (varname(ic:ic).eq.' ') varname(ic:ic)='_'
               endif
            enddo
            vardesc=description//', '//unitsr ! Last 9 chars. of "units"
c                                              get cut off (oh well)
            plchun=unitsr(1:24) ! last character of "units" gets cut off,
c                                and it's not in plotchar format (oh well)
            icd=1
            if (staggering.eq.'D   ') icd=0
            if (end_index(3).eq.mkzh+1) then ! interpolate to half sigma levels
               do k=1,mkzh
               do j=1,mjx-icd
               do i=1,miy-icd
                  scr3(i,j,k)=.5*(scr3(i,j,k)+scr3(i,j,k+1))
               enddo
               enddo
               enddo
               end_index(3)=mkzh
            endif
c            if (end_index(3).eq.mkzh) then
c               call writefile(scr3,varname,3,icd,vardesc,plchun,
c     &            fname,iendf1,ihrip,rhrip,chrip,
c     &            fullsigma,halfsigma,iexpanded,
c     &            iexpandedout,ioffexp,joffexp,miy,mjx,mkzh)
c            endif
c
c         Write out surface part from pressure level data.
c
            if (iprog.eq.2.or.iprog.eq.3) then
               iendvarname=index(varname,' ')-1
               if (iendvarname.eq.-1) iendvarname=10
               iendvarname=min(iendvarname,8)
               varname=varname(1:iendvarname)//'_s'
c
c            Last 13 chars. of "units" get cut off (oh well)
c
               vardesc='SFC '//description//', '//unitsr
               plchun=unitsr(1:24) ! last character of "units" gets cut off,
c                                and it's not in plotchar format (oh well)
               icd=1
               if (staggering.eq.'D   ') icd=0
c               call writefile(scr2,varname,2,icd,vardesc,plchun,
c     &            fname,iendf1,ihrip,rhrip,chrip,
c     &            fullsigma,halfsigma,iexpanded,
c     &            iexpandedout,ioffexp,joffexp,miy,mjx,mkzh)
            endif
         elseif (ordering.eq.'YX  ') then ! unknown 2d field
            print *,'unknown 2d field'
            read(iudatin) scr2
            varname=name
            inname="0"
            do ic=10,1,-1
               if (inname.eq."0") then
                  if (varname(ic:ic).ne.' ') inname="1"
               else
                  if (varname(ic:ic).eq.' ') varname(ic:ic)='_'
               endif
            enddo
            vardesc=description//', '//unitsr ! Last 9 chars. of "units"
c                                              get cut off (oh well)
            plchun=unitsr(1:24) ! last character of "units" gets cut off,
c                                and it's not in plotchar format (oh well)
            icd=1
            if (staggering.eq.'D   ') icd=0
c            call writefile(scr2,varname,2,icd,vardesc,plchun,
c     &         fname,iendf1,ihrip,rhrip,chrip,
c     &         fullsigma,halfsigma,iexpanded,
c     &         iexpandedout,ioffexp,joffexp,miy,mjx,mkzh)
         elseif (name.eq.'SIGMAH   ') then
            read(iudatin)
            idiscard=1
            print*,'   Discarding 1-D SIGMAH array --',
     &         ' RIPDP read it earlier.'
         elseif (name.eq.'PRESSURE ') then
            read(iudatin)
            idiscard=1
            if (iprog.le.3) then
               print*,'   Discarding 1-D PRESSURE array --',
     &            ' RIPDP read it earlier.'
            else
               print*,'   Discarding 1-D PRESSURE array --',
     &            ' not needed for sigma-level data.'
            endif
         else
            read(iudatin) varb
c            print *,varb
c            pause
            idiscard=1
            print*,'   Discarding ',name,
     &             '   because it is not 2- or 3-d data.'
         endif
 353     continue
         if (idiscard.eq.0) then
            print*,'Processing MM5 variable ',name
         endif
         goto 421
      elseif (flag.eq.2) then
         continue
      else
         print*,'Ran into a flag not =1 or 2 while reading data.'
         stop
      endif
c
c      endif
c
c   Read in der. q. data variables.
c
c      if (ipvdo.eq.1) then
c         call fillarray(qqn,miy*mjx*mkzh*nvarq,0.)
c         call fillarray(tqn,miy*mjx*mkzh*nvartq,0.)
c         call fillarray(uqn,miy*mjx*mkzh*nvarvq,0.)
c         call fillarray(vqn,miy*mjx*mkzh*nvarvq,0.)
c         do ivq=1,nvarq
c            read(iuqin) (((qqn(i,j,k,ivq),i=miyqb,miyqe),
c     &                     j=mjxqb,mjxqe),k=1,mkzh)
c            do k=1,mkzh
c               do j=mjxqb,mjxqe
c                  qqn(miyqe,j,k,ivq)=0.
c               enddo
c               do i=miyqb,miyqe-1
c                  qqn(i,mjxqe,k,ivq)=0.
c               enddo
c            enddo
c         enddo
c         do ivq=1,nvartq
c            read(iuqin) (((tqn(i,j,k,ivq),i=miyqb,miyqe),
c     &                     j=mjxqb,mjxqe),k=1,mkzh)
c            do k=1,mkzh
c               do j=mjxqb,mjxqe
c                  tqn(miyqe,j,k,ivq)=0.
c               enddo
c               do i=miyqb,miyqe-1
c                  tqn(i,mjxqe,k,ivq)=0.
c               enddo
c            enddo
c         enddo

c         if (xtime.gt.6.5.and.xtime.lt.30.5) then
c            ntq4av=ntq4av+1
c            do k=1,mkzh
c            do j=mjxqb,mjxqe-1
c            do i=miyqb,miyqe-1
c               tq4av(i,j,k)=tq4av(i,j,k)+tqn(i,j,k,5)
c            enddo
c            enddo
c            enddo
c         endif
c         do ivq=1,nvarvq
c            read(iuqin) (((uqn(i,j,k,ivq),i=miyqb,miyqe),
c     &                     j=mjxqb,mjxqe),k=1,mkzh)
c         enddo
c         do ivq=1,nvarvq
c            read(iuqin) (((vqn(i,j,k,ivq),i=miyqb,miyqe),
c     &                     j=mjxqb,mjxqe),k=1,mkzh)
c         enddo
c         call jumpendfile(iuqin,1)
c      endif
c
c   Read in observations data
c
c      if (iobsprc.eq.1.and.iobsprcfnd.eq.1) then
c         read(iuobsprc) rrbo
c         vardesc='Obs. precip. since last output time, mm'
c         plchun='mm'
c         call writefile(rrbo,'rrbo      ',2,1,vardesc,plchun,
c     &      fname,iendf1,ihrip,rhrip,chrip,
c     &      fullsigma,halfsigma,iexpanded,
c     &      iexpandedout,ioffexp,joffexp,miy,mjx,mkzh)
c      endif
c      if (iobscnv.eq.1.and.iobsclgfnd.eq.1) then
c         read(iuobsclg) clgo
c         vardesc='Obs. cloud ceiling, feet'
c         plchun='ft'
c         call writefile(clgo,'clgo      ',2,1,vardesc,plchun,
c     &      fname,iendf1,ihrip,rhrip,chrip,
c     &      fullsigma,halfsigma,iexpanded,
c     &      iexpandedout,ioffexp,joffexp,miy,mjx,mkzh)
c      endif
c      if (iobscnv.eq.1.and.iobsvisfnd.eq.1) then
c         read(iuobsvis) viso
c         vardesc='Obs. visibility, miles'
c         plchun='mi'
c         call writefile(viso,'viso      ',2,1,vardesc,plchun,
c     &      fname,iendf1,ihrip,rhrip,chrip,
c     &      fullsigma,halfsigma,iexpanded,
c     &      iexpandedout,ioffexp,joffexp,miy,mjx,mkzh)
c      endif
c      if (iftcnv.eq.1.and.iftclgfnd.eq.1) then
c         read(iuftclg) clgf
c         vardesc='FT-fcst. cloud ceiling, feet'
c         plchun='ft'
c         call writefile(clgf,'clgf      ',2,1,vardesc,plchun,
c     &      fname,iendf1,ihrip,rhrip,chrip,
c     &      fullsigma,halfsigma,iexpanded,
c     &      iexpandedout,ioffexp,joffexp,miy,mjx,mkzh)
c      endif
c      if (iftcnv.eq.1.and.iftvisfnd.eq.1) then
c         read(iuftvis) visf
c         vardesc='FT-fcst. visibility, miles'
c         plchun='mi'
c         call writefile(visf,'visf      ',2,1,vardesc,plchun,
c     &      fname,iendf1,ihrip,rhrip,chrip,
c     &      fullsigma,halfsigma,iexpanded,
c     &      iexpandedout,ioffexp,joffexp,miy,mjx,mkzh)
c      endif
c
      print*,'   Finished reading data for this time.'
c
c   Write out some fields that are OK as is.
      if ((igotitter.eq.1).and.(itime.eq.0)) then
c         vardesc='terrain height AMSL at cross points, m'
c         plchun='m'
         if (ioflg(27).eq.1) then
            k = 27
            print *,'k= ',k
            sf = 1.
            i2s(1) = 1
            i2s(2) = 1
            i2l(1) = mjx-1
            i2l(2) = miy-1
            call write2d_base(cdfid,varid(k),i2s,i2l,ter,miy,mjx,sf)
         end if
c         vardesc='terrain height AMSL at dot points, m'
c         plchun='m'
         if (ioflg(28).eq.1) then
            k = 28
            print *,'k= ',k
            sf = 1.
            i2s(1) = 1
            i2s(2) = 1
            i2l(1) = mjx
            i2l(2) = miy
            call write2d_base(cdfid,varid(k),i2s,i2l,dter,miy,mjx,sf)
         end if
      endif
      if ((igotitxlat.eq.1).and.(itime.eq.0)) then
c         vardesc='latitude at cross points, degrees'
c         plchun='m'
         if (ioflg(34).eq.1) then
            k = 34
            print *,'k= ',k
            sf = 1.
            i2s(1) = 1
            i2s(2) = 1
            i2l(1) = mjx-1
            i2l(2) = miy-1
            call write2d_base(cdfid,varid(k),i2s,i2l,xlat,miy,mjx,sf)
         end if
      endif
      if ((igotitxlon.eq.1).and.(itime.eq.0)) then
c         vardesc='longitude at cross points, degrees'
c         plchun='m'
         if (ioflg(35).eq.1) then
            k = 35
            print *,'k= ',k
            sf = 1.
            i2s(1) = 1
            i2s(2) = 1
            i2l(1) = mjx-1
            i2l(2) = miy-1
            call write2d_base(cdfid,varid(k),i2s,i2l,xlon,miy,mjx,sf)
         end if
      endif
      if ((igotitxmap.eq.1).and.(itime.eq.0)) then
c         vardesc='map factor on cross points'
c         plchun='none'
         if (ioflg(29).eq.1) then
            k = 29
            print *,'k= ',k
            sf = 1.
            i2s(1) = 1
            i2s(2) = 1
            i2l(1) = mjx-1
            i2l(2) = miy-1
            call write2d_base(cdfid,varid(k),i2s,i2l,xmap,miy,mjx,sf)
         end if
      endif
      if ((igotitdmap.eq.1).and.(itime.eq.0)) then
c         vardesc='map factor on dot points'
c         plchun='none'
         if (ioflg(30).eq.1) then
            k = 30
            print *,'k= ',k
            sf = 1.
            i2s(1) = 1
            i2s(2) = 1
            i2l(1) = mjx
            i2l(2) = miy
            call write2d_base(cdfid,varid(k),i2s,i2l,dmap,miy,mjx,sf)
         end if
      endif
c
c   Fill outer row and column of xlus with values from next
c      inner row or column.
c
      if ((igotitxlus.eq.1).and.(itime.eq.0)) then
         do j=2,mjx-2
            xlus(1,j)=xlus(2,j)
            xlus(miy-1,j)=xlus(miy-2,j)
         enddo
         do i=1,miy-1
            xlus(i,1)=xlus(i,2)
            xlus(i,mjx-1)=xlus(i,mjx-2)
         enddo
c         vardesc='land use category'
c         plchun='none'
         if (ioflg(36).eq.1) then
            k = 36
            print *,'k= ',k
            sf = 1.
            i2s(1) = 1
            i2s(2) = 1
            i2l(1) = mjx
            i2l(2) = miy
            call write2d_base(cdfid,varid(k),i2s,i2l,xlus,miy,mjx,sf)
         end if
      endif

c
c   Write xcor,dcor
c
      if ((igotitdotcor.eq.1).and.(itime.eq.0)) then
c         vardesc='coriolis parameter at dot points, per second'
c         plchun='s~S~-1~N~'
         if (ioflg(31).eq.1) then
            k = 31
            print *,'k= ',k
            sf = 1.
            i2s(1) = 1
            i2s(2) = 1
            i2l(1) = mjx
            i2l(2) = miy
         call write2d_base(cdfid,varid(k),i2s,i2l,dotcor,miy,mjx,sf)
         end if
c         vardesc='coriolis parameter at cross points, per second'
c         plchun='s~S~-1~N~'
         if (ioflg(32).eq.1) then
            k = 32
            print *,'k= ',k
            sf = 1.
            i2s(1) = 1
            i2s(2) = 1
            i2l(1) = mjx-1
            i2l(2) = miy-1
            call write2d_base(cdfid,varid(k),i2s,i2l,cor,miy,mjx,sf)
         end if
      endif

c print out reservoir temp

      if ((igotitrtmp.eq.1).and.(itime.eq.0)) then
c         vardesc='reservoir tmp'
c         plchun='s~S~-1~N~'
         if (ioflg(33).eq.1) then
            k = 33
            print *,'k= ',k
            sf = 1.
            i2s(1) = 1
            i2s(2) = 1
            i2l(1) = mjx-1
            i2l(2) = miy-1
            call write2d_base(cdfid,varid(k),i2s,i2l,cor,miy,mjx,sf)
         end if
      endif

c
c   May need to combine ground temperature and SST
c
      if (igotittgk.eq.1.or.igotitsst.eq.1) then
         if (igotittgk.eq.0) then
            do j=1,mjx-1
            do i=1,miy-1
               tgk(i,j)=sst(i,j)
            enddo
            enddo
         elseif (igotittgk.eq.1.and.igotitsst.eq.1) then
            if (igotitxlus.eq.1) then
               if (ilandset.eq.1) then
                  iwater=7
               elseif (ilandset.eq.2) then
                  iwater=16
               elseif (ilandset.eq.3) then
                  iwater=15
               endif
               do j=1,mjx-1
               do i=1,miy-1
                  if (nint(xlus(i,j)).eq.iwater) tgk(i,j)=sst(i,j)
               enddo
               enddo
            else
               print*,'Cannot discriminate between ground temp. and'
               print*,'SST without land use information.'
               stop
            endif
         endif
c         vardesc='Ground/sea-surface temperature, K'
c         plchun='K'
c         vardesc='ground (or sea-surface) temperature, K'
c         plchun='K'
         if (ioflg(23).eq.1) then
            k = 23
            print *,'k= ',k
            sf = 1.
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write2d(cdfid,varid(k),i4s,i4l,tgk,miy,mjx,sf)
         end if
         if (ioflg(24).eq.1) then
            k = 24
            print *,'k= ',k
            sf = 1.
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write2d(cdfid,varid(k),i4s,i4l,sst,miy,mjx,sf)
         end if
      endif
c
c   SNO might either be either snow cover (1=snow,0=no snow) or
c   snow depth in mm.
c
      if ((igotitsno.eq.1).and.(itime.eq.0)) then
         inotzeroorone=0
         do j=2,mjx-2
         do i=2,miy-2
            icheck=nint(sno(i,j)*10.)
            if (icheck.gt.0.and.icheck.ne.10.and.icheck.ne.5)
     &          inotzeroorone=inotzeroorone+1
         enddo
         enddo
         if (inotzeroorone.gt.0) then ! it's snow depth
c            varname='snod      '
c            vardesc='Snow depth, mm'
c            plchun='mm'
             print *,'snow depth'
         else                         ! it's snow cover
            do j=2,mjx-2
            do i=2,miy-2
               if (sno(i,j).ne.1.) sno(i,j)=0.
            enddo
            enddo
c            varname='sno       '
c            vardesc='Snow cover (0.0 or 1.0)'
c            plchun='none'
             print *,'snow cover'
         endif
        if (ioflg(37).eq.1) then
            k = 37
            print *,'k= ',k
            sf = 1.
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write2d(cdfid,varid(k),i4s,i4l,sno,miy,mjx,sf)
         end if
      endif
c
c   If we got SLP, convert it to hPa and write out      
c
      if (igotitslp.eq.1) then
c         vardesc='Sea-level pressure, hPa'
c         plchun='hPa'
         do j=1,mjx-1
         do i=1,miy-1
            slp(i,j)=.01*slp(i,j)
         enddo
         enddo
        if (ioflg(22).eq.1) then
            k = 22
            print *,'k= ',k
            sf = 1.
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write2d(cdfid,varid(k),i4s,i4l,slp,miy,mjx,sf)
         end if
      endif
c
c   Write out some extra 2-D fields, if they were found.
c
      if (igotitpblh.eq.1) then
c         vardesc='blackadar pbl height, m'
c         plchun='m'
         if (ioflg(38).eq.1) then
            k = 38
            print *,'k= ',k
            sf = 1.
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write2d(cdfid,varid(k),i4s,i4l,pblh,miy,mjx,sf)
         end if
      endif
      if (igotitregime.eq.1) then
c         vardesc='blackadar pbl regime'
c         plchun='none'
         if (ioflg(39).eq.1) then
            k = 39
            print *,'k= ',k
            sf = 1.
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write2d(cdfid,varid(k),i4s,i4l,regime,miy,mjx,sf)
         end if
      endif
      if (igotitsshflx.eq.1) then
c         vardesc='short wave flux, W/m**2'
c         plchun='W/m**2 s~S~-1~N~'
         if (ioflg(40).eq.1) then
            k = 40
            print *,'k= ',k
            sf = 1.
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write2d(cdfid,varid(k),i4s,i4l,sshflx,miy,mjx,sf)
         end if
      endif
      if (igotitslhflx.eq.1) then
c         vardesc='long wave flux, W/m**2'
c         plchun='W/m**2 s~S~-1~N~'
         if (ioflg(41).eq.1) then
            k = 41
            print *,'k= ',k
            sf = 1.
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write2d(cdfid,varid(k),i4s,i4l,slhflx,miy,mjx,sf)
         end if
      endif
      if (igotitxmav.eq.1) then
c         vardesc='Soil moisture availability, %'
c         plchun='%'
         if (ioflg(43).eq.1) then
            k = 43
            print *,'k= ',k
            sf = 1.
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write2d(cdfid,varid(k),i4s,i4l,xmav,miy,mjx,sf)
         end if
      endif
      if (igotitust.eq.1) then
c         vardesc='ust'
c         plchun=' s~S~-1~N~'
         if (ioflg(42).eq.1) then
            k = 42
            print *,'k= ',k
            sf = 1.
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write2d(cdfid,varid(k),i4s,i4l,ust,miy,mjx,sf)
         end if
      endif
      if (igotitswdown.eq.1) then
c         vardesc='short wave downward flux, W/m**2'
c         plchun='W/m**2 s~S~-1~N~'
         if (ioflg(44).eq.1) then
            k = 44
            print *,'k= ',k
            sf = 1.
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write2d(cdfid,varid(k),i4s,i4l,swdown,miy,mjx,sf)
         end if
      endif
      if (igotitlwdown.eq.1) then
c         vardesc='long wave downward flux, W/m**2'
c         plchun='W/m**2 s~S~-1~N~'
         if (ioflg(45).eq.1) then
            k = 45
            print *,'k= ',k
            sf = 1.
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write2d(cdfid,varid(k),i4s,i4l,lwdown,miy,mjx,sf)
         end if
      endif
      if (igotitswout.eq.1) then
c         vardesc='short wave outgoing flux, W/m**2'
c         plchun='W/m**2 s~S~-1~N~'
         if (ioflg(46).eq.1) then
            k = 46
            print *,'k= ',k
            sf = 1.
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write2d(cdfid,varid(k),i4s,i4l,swout,miy,mjx,sf)
         end if
      endif
      if (igotitlwout.eq.1) then
c         vardesc='long wave outgoing flux, W/m**2'
c         plchun='W/m**2 s~S~-1~N~'
         if (ioflg(47).eq.1) then
            k = 47
            print *,'k= ',k
            sf = 1.
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write2d(cdfid,varid(k),i4s,i4l,lwout,miy,mjx,sf)
         end if
      endif
      if (igotitsoil1.eq.1) then
c         vardesc='soil temp layer 1, K'
c         plchun='K s~S~-1~N~'
         if (ioflg(48).eq.1) then
            k = 48
            print *,'k= ',k
            sf = 1.
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write2d(cdfid,varid(k),i4s,i4l,soil1,miy,mjx,sf)
         end if
      endif
      if (igotitsoil2.eq.1) then
c         vardesc='soil temp layer 2, K'
c         plchun='K s~S~-1~N~'
         if (ioflg(49).eq.1) then
            k = 49
            print *,'k= ',k
            sf = 1.
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write2d(cdfid,varid(k),i4s,i4l,soil2,miy,mjx,sf)
         end if
      endif
      if (igotitsoil3.eq.1) then
c         vardesc='soil temp layer 3, K'
c         plchun='K s~S~-1~N~'
         if (ioflg(50).eq.1) then
            k =50
            print *,'k= ',k
            sf = 1.
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write2d(cdfid,varid(k),i4s,i4l,soil3,miy,mjx,sf)
         end if
      endif
      if (igotitsoil4.eq.1) then
c         vardesc='soil temp layer 4, K'
c         plchun='K s~S~-1~N~'
         if (ioflg(51).eq.1) then
            k = 51
            print *,'k= ',k
            sf = 1.
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write2d(cdfid,varid(k),i4s,i4l,soil4,miy,mjx,sf)
         end if
      endif
      if (igotitsoil5.eq.1) then
c         vardesc='soil temp layer 5, K'
c         plchun='K s~S~-1~N~'
         if (ioflg(52).eq.1) then
            k = 52
            print *,'k= ',k
            sf = 1.
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write2d(cdfid,varid(k),i4s,i4l,soil5,miy,mjx,sf)
         end if
      endif
      if (igotitsoil6.eq.1) then
c         vardesc='soil temp 6, K'
c         plchun='K s~S~-1~N~'
         if (ioflg(53).eq.1) then
            k = 53
            print *,'k= ',k
            sf = 1.
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write2d(cdfid,varid(k),i4s,i4l,soil6,miy,mjx,sf)
         end if
      endif
      if (igotitt2.eq.1) then
c         vardesc='2m temp, K'
c         plchun='K s~S~-1~N~'
         if (ioflg(54).eq.1) then
            k = 54
            print *,'k= ',k
            sf = 1.
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write2d(cdfid,varid(k),i4s,i4l,t2,miy,mjx,sf)
         end if
      endif
      if (igotitq2.eq.1) then
c         vardesc='2m RH, %'
c         plchun='%'
         if (ioflg(55).eq.1) then
            k = 55
            print *,'k= ',k
            sf = 1.
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write2d(cdfid,varid(k),i4s,i4l,q2,miy,mjx,sf)
         end if
      endif
      if (igotitu10.eq.1) then
c         vardesc='10m u-comp, m/s'
c         plchun='m/s'
         if (ioflg(56).eq.1) then
            k = 56
            print *,'k= ',k
            sf = 1.
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write2d(cdfid,varid(k),i4s,i4l,u10,miy,mjx,sf)
         end if
      endif
      if (igotitv10.eq.1) then
c         vardesc='10m v-comp, m/s'
c         plchun='m/s'
         if (ioflg(57).eq.1) then
            k = 57
            print *,'k= ',k
            sf = 1.
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write2d(cdfid,varid(k),i4s,i4l,v10,miy,mjx,sf)
         end if
      endif
c
c other fields, possibly no longer used
c

c      if ((igotitznt.eq.1).and.(itime.eq.0)) then
cc         vardesc='roughness length, m'
cc         plchun='m'
c         if (ioflg(26).eq.1) then
c            k = 26
c            print *,k
c            sf = 1.
c            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
c            call write2d(cdfid,varid(k),i4s,i4l,znt,miy,mjx,sf)
c         end if
c      endif
c      if (igotithfx.eq.1) then
cc         vardesc='upward surface sensible heat flux, W/m**2'
cc         plchun='W m~S~-2~N~'
c         if (ioflg(27).eq.1) then
c            k = 27
c            print *,k
c            sf = 1.
c            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
c            call write2d(cdfid,varid(k),i4s,i4l,hfx,miy,mjx,sf)
c         end if
c      endif
c      if (igotitrmol.eq.1) then
cc         vardesc='monin length, m'
cc         plchun='m'
c         if (ioflg(30).eq.1) then
c            k = 30
c            print *,k
c            sf = 1.
c            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
c            call write2d(cdfid,varid(k),i4s,i4l,rmol,miy,mjx,sf)
c         end if
c      endif
c      if (igotithlfx.eq.1) then
cc         vardesc='upward surface latent heat flux, W/m**2'
cc         plchun='W m~S~-2~N~'
c         if (ioflg(31).eq.1) then
c            k = 31
c            print *,k
c            sf = 1.
c            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
c            call write2d(cdfid,varid(k),i4s,i4l,hlfx,miy,mjx,sf)
c         end if
c      endif
c      if (igotitxmfx.eq.1) then
cc         vardesc='upward surface flux of positive x-momentum, N/m**2'
cc         plchun='N m~S~-2~N~'
c         if (ioflg(32).eq.1) then
c            k = 32
c            print *,k
c            sf = 1.
c            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
c            call write2d(cdfid,varid(k),i4s,i4l,xmfx,miy,mjx,sf)
c         end if
c      endif
c      if (igotitymfx.eq.1) then
cc         vardesc='upward surface flux of positive y-momentum, N/m**2'
cc         plchun='N m~S~-2~N~'
c         if (ioflg(33).eq.1) then
c            k = 33
c            print *,k
c            sf = 1.
c            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
c            call write2d(cdfid,varid(k),i4s,i4l,ymfx,miy,mjx,sf)
c         end if
c      endif
c      if (igotitrnfx.eq.1) then
cc         vardesc='net upward surface radiative flux, W/m**2'
cc         plchun='W m~S~-2~N~'
c         if (ioflg(34).eq.1) then
c            k = 34
c            print *,k
c            sf = 1.
c            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
c            call write2d(cdfid,varid(k),i4s,i4l,rnfx,miy,mjx,sf)
c         end if
c      endif
c      if (igotitswh.eq.1) then
cc         vardesc='significant wave height, m'
cc         plchun='m'
c         if (ioflg(35).eq.1) then
c            k = 35
c            print *,k
c            sf = 1.
c            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
c            call write2d(cdfid,varid(k),i4s,i4l,swh,miy,mjx,sf)
c         end if
c      endif
c      if (igotitcxwave.eq.1) then
cc         vardesc='x-component of wave phase velocity, m/s'
cc         plchun='m s~S~-1~N~'
c         if (ioflg(36).eq.1) then
c            k = 36
c            print *,k
c            sf = 1.
c            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
c            call write2d(cdfid,varid(k),i4s,i4l,cxwave,miy,mjx,sf)
c         end if
c      endif
c      if (igotitcywave.eq.1) then
cc         vardesc='y-component of wave phase velocity, m/s'
cc         plchun='m s~S~-1~N~'
c         if (ioflg(27).eq.1) then
c            k = 27
c            print *,k
c            sf = 1.
c            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
c            call write2d(cdfid,varid(k),i4s,i4l,cywave,miy,mjx,sf)
c         end if
c      endif
c      if (igotitueff.eq.1) then
cc         vardesc='effective u-velocity wrt roughness elements, m/s'
cc         plchun='m s~S~-1~N~'
c          if (ioflg(38).eq.1) then
c            k = 38
c            print *,k
c            sf = 1.
c            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
c            call write2d(cdfid,varid(k),i4s,i4l,ueff,miy,mjx,sf)
c         end if
c      endif
c      if (igotitveff.eq.1) then
cc         vardesc='effective v-velocity wrt roughness elements, m/s'
cc         plchun='m s~S~-1~N~'
c         if (ioflg(39).eq.1) then
c            k = 39
c            print *,k
c            sf = 1.
c            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
c            call write2d(cdfid,varid(k),i4s,i4l,veff,miy,mjx,sf)
c         end if
c      endif

c
c   Create pstd
c
      if (iprog.eq.1.or.iprog.eq.2.or.iprog.eq.3) then
c
c      Make pstx in Pa, to be consistent with pstx that is read in
c      from V3 sigma-level output
c
         do j=1,mjx
         do i=1,miy
            pstx(i,j)=100.*pstarconst
         enddo
         enddo
         igotitpstx=1
      endif
      if (igotitpstx.eq.1) then
         do j=1,mjx
         do i=1,miy
            iph=min(i,miy-1)
            jph=min(j,mjx-1)
            imh=max(i-1,1)
            jmh=max(j-1,1)
            pstd(i,j)=.25*(pstx(iph,jph)+pstx(imh,jph)+
     &                     pstx(iph,jmh)+pstx(imh,jmh))
         enddo
         enddo
      else
         print*,'Didn''t find pstx.  Stopping'
         stop
      endif
c
c   Process and write out other variables
c
      if (igotituuu_sfan.eq.1) then
c         vardesc='u-velocity, m/s'
c         plchun='m s~S~-1~N~'
         if (ioflg(1).eq.1) then
            k = 1
            print *,'k= ',k
            sf = 1.
            i2s(1) = 1
            i2s(2) = 1
            i2l(1) = mjx-1
            i2l(2) = miy-1
            call setfld(ndim(k),grid(k),i2s,i2l,itime,miy,mjx,mkzh)
           call write2d_base(cdfid,varid(k),i2s,i2l,uuu_sfan,miy,mjx,sf)
         end if
      endif
      if (igotituuu.eq.1) then
c         vardesc='u-velocity, m/s'
c         plchun='m s~S~-1~N~'
         if (ioflg(2).eq.1) then
            k = 2
            print *,'k= ',k
            sf = 1.
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write3d(cdfid,varid(k),i4s,i4l,uuu,miy,mjx,sf)
         end if
      endif
      if (igotitvvv_sfan.eq.1) then
c         vardesc='v-velocity, m/s'
c         plchun='m s~S~-1~N~'
         if (ioflg(3).eq.1) then
            k = 3
            print *,'k= ',k 
            sf = 1.
            i2s(1) = 1
            i2s(2) = 1
            i2l(1) = mjx-1
            i2l(2) = miy-1
            call setfld(ndim(k),grid(k),i2s,i2l,itime,miy,mjx,mkzh)
           call write2d_base(cdfid,varid(k),i2s,i2l,vvv_sfan,miy,mjx,sf)
         end if
      endif
      if (igotitvvv.eq.1) then
c         vardesc='v-velocity, m/s'
c         plchun='m s~S~-1~N~'
         if (ioflg(4).eq.1) then
            k = 4
            print *,'k= ',k 
            sf = 1.
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write3d(cdfid,varid(k),i4s,i4l,vvv,miy,mjx,sf)
         end if
      endif
      if (igotittmk_sfan.eq.1) then
c         vardesc='temperature, K'
c         plchun='K'
         if (ioflg(5).eq.1) then
            k = 5
            print *,'k= ',k
            sf = 1.
            i2s(1) = 1
            i2s(2) = 1
            i2l(1) = mjx-1
            i2l(2) = miy-1
            call setfld(ndim(k),grid(k),i2s,i2l,itime,miy,mjx,mkzh)
           call write2d_base(cdfid,varid(k),i2s,i2l,tmk_sfan,miy,mjx,sf)
         end if
      endif
      if (igotittmk.eq.1) then
c         vardesc='temperature, K'
c         plchun='K'
         if (ioflg(6).eq.1) then
            k = 6
            print *,'k= ',k
            sf = 1.
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write3d(cdfid,varid(k),i4s,i4l,tmk,miy,mjx,sf)
         end if
      endif
      if (igotitqvp.eq.1) then
c         vardesc='mixing ratio of water vapor, g/kg'
c         plchun='g kg~S~-1~N~'
         if (ioflg(7).eq.1) then
            k = 7
            print *,'k= ',k
            sf = .001
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write3d(cdfid,varid(k),i4s,i4l,qvp,miy,mjx,sf)
         endif
      end if
      if (igotitrh.eq.1) then
c         vardesc='Relative humidity (w.r.t. water), %'
c         plchun='%'
         if (ioflg(8).eq.1) then
            k = 8
            print *,'k= ',k
            sf = 1.
            i2s(1) = 1
            i2s(2) = 1
            i2l(1) = mjx-1
            i2l(2) = miy-1
            call setfld(ndim(k),grid(k),i2s,i2l,itime,miy,mjx,mkzh)
           call write2d_base(cdfid,varid(k),i2s,i2l,qvp_sfan,miy,mjx,sf)
         endif
c
c      Convert to mixing ratio (g/kg) and write out again
c
         if ((iprog.eq.2.or.iprog.eq.3).and.igotittmk.eq.1) then
            do k=1,mkzh
            do j=1,mjx-1
            do i=1,miy-1
               es=ezero*exp(eslcon1*(tmk(i,j,k)-celkel)/
     &            (tmk(i,j,k)-eslcon2))
               ws=eps*es/(prslvl(k)-es)  ! in kg/kg
c
c            Convert qvp from RH in % to mix rat in g/kg
c
               qvp(i,j,k)=10.*qvp(i,j,k)*ws
c diff test
c               qvp(i,j,k)=qvp(i,j,k)+2.
            enddo
            enddo
            enddo
         endif
c         vardesc='Water vapor mixing ratio, g/kg'
c         plchun='g kg~S~-1~N~'
         if (ioflg(7).eq.1) then
            k = 7
            print *,'k= ',k
            sf = .001
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write3d(cdfid,varid(k),i4s,i4l,qvp,miy,mjx,sf)
         endif
         igotitqvp=1
      endif
      if (igotitqcw.eq.1) then
c         vardesc='mixing ratio of cloud water, g/kg'
c         plchun='g kg~S~-1~N~'
         if (ioflg(9).eq.1) then
            k = 9
            print *,'k= ',k
            sf = .001
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write3d(cdfid,varid(k),i4s,i4l,qcw,miy,mjx,sf)
         end if
      endif
      if (igotitqra.eq.1) then
c         vardesc='mixing ratio of rain, g/kg'
c         plchun='g kg~S~-1~N~'
         if (ioflg(10).eq.1) then
            k = 10
            print *,'k= ',k
            sf = .001
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write3d(cdfid,varid(k),i4s,i4l,qra,miy,mjx,sf)
         end if
      endif
      if (igotitwww.eq.1) then
c
c      Decouple www, interpolate it to half sigma levels
c      and convert it to cm/s.
c
         do k=1,mkzh
         do j=1,mjx-1
         do i=1,miy-1
            www(i,j,k)=50.*(www(i,j,k)+www(i,j,k+1))
         enddo
         enddo
         enddo
c         vardesc='vertical velocity, cm/s'
c         plchun='cm s~S~-1~N~'
         if (ioflg(17).eq.1) then
            k = 17
            print *,'k= ',k
            sf = .01
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write3d(cdfid,varid(k),i4s,i4l,www,miy,mjx,sf)
         end if
      endif
c
      if (iprog.eq.5.or.iprog.eq.6.or.iprog.eq.11) then
c
c   Note, in the above "if" statement,
c   that pressure will not be processed if iprog=1,2 or 3.  If
c   iprog=2 or 3, pressure can be easily created in rip, since
c   data levels are on const. pressure surfaces.
c
      if ((inhyd.eq.1.and.igotitprs.eq.1.and.igotitpstx.eq.1).or.
     &    (inhyd.eq.0.and.igotitprs.eq.0.and.igotitpstx.eq.1)) then
c
c      Convert prs from pressure perturbation in Pa to total pressure
c      in mb.
c
         igotitprs=1
         do k=1,mkzh
         do j=1,mjx-1
         do i=1,miy-1
             prs(i,j,k)=sigh(k)*pstx(i,j)*.01+ptop+
     &            .01*prs(i,j,k)  ! want in hPa
         enddo
         enddo
         enddo
c         vardesc='pressure, mb'
c         plchun='mb'
         if (ioflg(18).eq.1) then
            k = 18
            print *,'k= ',k
            sf = 100.
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write3d(cdfid,varid(k),i4s,i4l,prs,miy,mjx,sf)
         end if
      elseif (inhyd.eq.1) then
         print*,'That is odd:  data is supposedly nonhydrostatic,'
         print*,'but no pressure perturbation array was found.'
         stop
      elseif (inhyd.eq.0) then
         print*,'That is odd:  data is supposedly hydrostatic,'
         print*,'but a pressure perturbation array was found.'
         stop
      endif
      endif
c
      if (igotitqci.eq.1) then
c         vardesc='mixing ratio of cloud ice, g/kg'
c         plchun='g kg~S~-1~N~'
         if (ioflg(11).eq.1) then
            k = 11
            print *,'k= ',k
            sf = .001
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write3d(cdfid,varid(k),i4s,i4l,qci,miy,mjx,sf)
         end if
      endif
      if (igotitqsn.eq.1) then
c         vardesc='mixing ratio of snow, g/kg'
c         plchun='g kg~S~-1~N~'
         if (ioflg(12).eq.1) then
            k = 12
            print *,'k= ',k
            sf = .001
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write3d(cdfid,varid(k),i4s,i4l,qsn,miy,mjx,sf)
         end if
      endif
      if (igotitqgr.eq.1) then
c         vardesc='mixing ratio of graupel, g/kg'
c         plchun='g kg~S~-1~N~'
         if (ioflg(13).eq.1) then
            k = 13
            print *,'k= ',k
            sf = .001
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write3d(cdfid,varid(k),i4s,i4l,qgr,miy,mjx,sf)
         end if
      endif
c
      if (igotitrnci.eq.1) then
c         vardesc='mixing ratio of graupel, g/kg'
c         plchun='g kg~S~-1~N~'
         if (ioflg(14).eq.1) then
            k = 14
            print *,'k= ',k
            sf = .001
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write3d(cdfid,varid(k),i4s,i4l,rnci,miy,mjx,sf)
         end if
      endif
      if (igotittke.eq.1) then
c         vardesc='Turbulent Kinetic Energy'
c         plchun='j~kg'
         if (ioflg(15).eq.1) then
            k = 15
            print *,'k= ',k
            sf = 1.
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write3d(cdfid,varid(k),i4s,i4l,tke,miy,mjx,sf)
         end if
      endif 
      if (igotitradtnd.eq.1) then
c         vardesc='radiation tendecny, g/kg'
c         plchun='W/m**2~S~-1~N~'
         if (ioflg(16).eq.1) then
            k = 16
            print *,'k= ',k
            sf = 1.
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write3d(cdfid,varid(k),i4s,i4l,radtnd,miy,mjx,sf)
         end if
      endif
c
      if ((igotitpstx.eq.1).and.(itime.eq.0)) then
         do j=1,mjx-1
         do i=1,miy-1
            pstx(i,j)=pstx(i,j)*.01  ! convert to mb before writing
         enddo
         enddo
c         vardesc='p-star on cross points, mb'
c         plchun='mb'
         if (ioflg(20).eq.1) then
            k = 20
            print *,'k= ',k
            sf = 100.
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write2d(cdfid,varid(k),i4s,i4l,pstx,miy,mjx,sf)
         end if
c     &      fullsigma,halfsigma,miy,mjx,mkzh)
      endif
      if ((igotitpstx.eq.1).and.(itime.eq.0)) then
         do j=1,mjx
         do i=1,miy
            pstd(i,j)=pstd(i,j)*.01  ! convert to mb before writing
         enddo
         enddo
c         vardesc='p-star on dot points, mb'
c         plchun='mb'
         if (ioflg(21).eq.1) then
            k = 21
            print *,'k= ',k
            sf = 100.
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write2d(cdfid,varid(k),i4s,i4l,pstd,miy,mjx,sf)
         end if
      endif

      if (iprog.gt.3.and.igotitpstx.eq.1.and.igotitprs.eq.1.and.
     &    igotittmk.eq.1.and.igotitter.eq.1.and.time.eq.0) then
c
c      Calculate ght.
c
         do k=1,mkzh
         do j=1,mjx
         do i=1,miy
            ght(i,j,k)=0.0
         enddo
         enddo
         enddo
         if (igotitqvp.ne.1) then
            do k=1,mkzh
            do j=1,mjx-1
            do i=1,miy-1
               qvp(i,j,k)=0.0
            enddo
            enddo
            enddo
         endif
         if (ioflg(59).eq.1) then
            k = 59
         call ghtcalc(sigh,pstd,prs,qvp,tmk,dter,ght,rgas,ussalr,
     &      grav,refslp,refslt,reflaps,refstratt,ptop,inhyd,
     &      miy,mjx,mkzh,grid(k))
c         vardesc='geopotential height at dot points, m'
c         plchun='m'
            print *,'k= ',k
            sf = 1.
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write3d(cdfid,varid(k),i4s,i4l,ght,miy,mjx,sf)
         end if
         if (ioflg(58).eq.1) then
            k = 58
         call ghtcalc(sigh,pstx,prs,qvp,tmk,ter,ght,rgas,ussalr,
     &      grav,refslp,refslt,reflaps,refstratt,ptop,inhyd,
     &      miy,mjx,mkzh,grid(k))
c         vardesc='geopotential height at cross points, m'
c         plchun='m'
            print *,'k= ',k
            sf = 1.
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write3d(cdfid,varid(k),i4s,i4l,ght,miy,mjx,sf)
         end if
      endif
      if (iprog.eq.2.or.iprog.eq.3) then
c
c      Special stuff to do for pressure level data.
c      First, get desired sigma level to use temperature from.
c
         sigc=(850.-ptop)/(1000.-ptop)
         do k=1,mkzh
            if (sigc.ge.sigf(k).and.sigc.le.sigf(k+1)) kupper=k
         enddo
         expon=rgas*ussalr/grav
         exponi=1./expon
c
         do j=1,mjx-1
         do i=1,miy-1
c
c      Create ter (i.e., height at the pseudo-surface defined by p=pbot).
c
         ter(i,j)=ght(i,j,mkzh)-rgas/grav*
     *      virtual(tmk(i,j,mkzh),.001*qvp(i,j,mkzh))*
     &      log(pbot/prslvl(mkzh))
         igotitter=1
c
c      Create prs_tsf (i.e., true surface pressure, at hgt=ter_tsf).
c
         zlhsl=ght(i,j,mkzh)   ! lhsl => "lowest half sigma level"
         ezlhsl=exp(-zlhsl/sclht)
         plhsl=prslvl(mkzh)
         zpsf=ter(i,j)   ! psf => "at the pseudo-surface"
         ezpsf=exp(-zpsf/sclht)
         ppsf=pbot
         ztsf=ter_tsf(i,j)   ! tsf => "at the true surface"
         eztsf=exp(-ztsf/sclht)
c        ptsf = prs_tsf(i,j) = ????  This is what we're after
c
c      First check if true surface is above top-most half sigma level.
c
         if (ztsf.gt.ght(i,j,1)) then
            print*,'True surface is above highest level of data.'
            print*,'i,j=',i,j
            print*,'This seems very strange.'
            print*,'ztsf,ght(i,j,1)=',ztsf,ght(i,j,1)
            stop
         endif
c
c      Check if true surface is somewhere within sigma levels.
c
         do k=mkzh,2,-1
            if (ztsf.ge.ght(i,j,k).and.
     &          ztsf.le.ght(i,j,k-1)) then
               ezk=exp(-ght(i,j,k)/sclht)
               ezkm1=exp(-ght(i,j,k-1)/sclht)
               prs_tsf(i,j)=((ezk-eztsf)*prslvl(k-1)+
     &                       (eztsf-ezkm1)*prslvl(k))/(ezk-ezkm1)
               goto 716
            endif
         enddo
c
c      Otherwise, true surface is below the lowest half sigma level
c
         if (ztsf.ge.zpsf) then
c
c         True surface is below the lowest half sigma level
c         but above the pseudo-surface.
c
            prs_tsf(i,j)=((ezpsf-eztsf)*plhsl+(eztsf-ezlhsl)*ppsf)/
     &         (ezpsf-ezlhsl)
c
         else
c
c         True surface is below the pseudo-surface.
c         Extrapolate using USSALR
c
            tpsfbg=tmk(i,j,kupper)*(ppsf/prslvl(kupper))**expon
            tpsfbgv=virtual(tpsfbg,.001*qvp(i,j,mkzh))
            prs_tsf(i,j)=ppsf*(1.+ussalr/tpsfbgv*(zpsf-ztsf))**exponi
c
         endif
c
 716     continue
c
         enddo
         enddo
c
c         vardesc='terrain height AMSL, m'
c         plchun='m'
         if (ioflg(27).eq.1) then
            k = 27
            print *,'k= ',k
            sf = 1.
            i2s(1) = 1
            i2s(2) = 1
            i2l(1) = mjx-1
            i2l(2) = miy-1
            call write2d_base(cdfid,varid(k),i2s,i2l,ter,miy,mjx,sf)
         end if
         vardesc='Pressure (at true sfc), hPa'
         plchun='hPa'
         if (ioflg(27).eq.1) then
            k = 27
            print *,'k= ',k
            sf = 1.
            i2s(1) = 1
            i2s(2) = 1
            i2l(1) = mjx-1
            i2l(2) = miy-1
            call write2d_base(cdfid,varid(k),i2s,i2l,prs_tsf,miy,mjx,sf)
         end if
c
c      Also, now that we have prs_tsf, we can process rhu_sfan and
c      qvp_sfan
c
         if (igotitrh_sfan.eq.1) then
c            vardesc='Relative humidity (sfc. anal.), %'
c            plchun='%'
           if (ioflg(8).eq.1) then
              k = 8
              print *,'k= ',k
              sf = 1.
              i2s(1) = 1
              i2s(2) = 1
              i2l(1) = mjx-1
              i2l(2) = miy-1
              call setfld(ndim(k),grid(k),i2s,i2l,itime,miy,mjx,mkzh)
           call write2d_base(cdfid,varid(k),i2s,i2l,rhu_sfan,miy,mjx,sf)
           endif
c
c         Convert to mixing ratio (g/kg) and write out again
c
            if ((iprog.eq.2.or.iprog.eq.3).and.igotittmk_sfan.eq.1) then
               do j=1,mjx-1
               do i=1,miy-1
                  es=ezero*exp(eslcon1*(tmk_sfan(i,j)-celkel)/
     &               (tmk_sfan(i,j)-eslcon2))
                  ws=eps*es/(prs_tsf(i,j)-es)  ! in kg/kg
c
c               Convert qvp from RH in % to mix rat in g/kg

                  qvp_sfan(i,j)=10.*qvp_sfan(i,j)*ws
               enddo
               enddo
            endif
c            vardesc='Water vapor mixing ratio (sfc. anal.), g/kg'
c            plchun='g kg~S~-1~N~'
           if (ioflg(7).eq.1) then
              k = 7
              print *,'k= ',k
              sf = 1.
              i2s(1) = 1
              i2s(2) = 1
              i2l(1) = mjx-1
              i2l(2) = miy-1
              call setfld(ndim(k),grid(k),i2s,i2l,itime,miy,mjx,mkzh)
           call write2d_base(cdfid,varid(k),i2s,i2l,qvp_sfan,miy,mjx,sf)
           endif
         endif
      endif

      if (igotitrtc.eq.1) then
         do j=1,mjx-1
         do i=1,miy-1
            rtc(i,j)=10.*rtc(i,j) ! cm to mm
            rrc(i,j)=rtc(i,j)-rpc(i,j)
            rrc(i,j)=amax1(rrc(1,j),0.)
            rpc(i,j)=rtc(i,j)
         enddo
         enddo
c         vardesc='cumulus precipitation since start of fcst., mm'
c         plchun='mm'
         if (ioflg(25).eq.1) then
            k = 25
            print *,'k= ',k
c            sf = .001
            sf = 1.
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write2d(cdfid,varid(k),i4s,i4l,rtc,miy,mjx,sf)
         end if
c         vardesc='cumulus precipitation since previous data time, mm'
c         plchun='mm'
         if (ioflg(60).eq.1) then
            k = 60
            print *,'k= ',k
c            sf = .001
            sf = 1.
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write2d(cdfid,varid(k),i4s,i4l,rrc,miy,mjx,sf)
         end if
      endif
      if (igotitrte.eq.1) then
         do j=1,mjx-1
         do i=1,miy-1
            rte(i,j)=10.*rte(i,j) ! cm to mm
            rre(i,j)=rte(i,j)-rpe(i,j)
            rre(i,j)=amax1(rre(1,j),0.)
            rpe(i,j)=rte(i,j)
         enddo
         enddo
c         vardesc='explicit precipitation since start of fcst., mm'
c         plchun='mm'
         if (ioflg(26).eq.1) then
            k = 26
            print *,'k= ',k
c            sf = .001       
            sf = 1.
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write2d(cdfid,varid(k),i4s,i4l,rte,miy,mjx,sf)
         end if
c         vardesc='explicit precipitation since previous data time, mm'
c         plchun='mm'
         if (ioflg(61).eq.1) then
            k = 61
            print *,'k= ',k
c            sf = .001
            sf = 1. 
            call setfld(ndim(k),grid(k),i4s,i4l,itime,miy,mjx,mkzh)
            call write2d(cdfid,varid(k),i4s,i4l,rre,miy,mjx,sf)
         end if
      endif
c
 990  continue
      itime = itime + 1
      goto 240       ! End of time loop.
c
 1000 continue
c
      call clsfile(cdfid)

      print*
      print*,'===================================='
      print*,'        Conversion Complete         '
      print*,'===================================='
      return
      end
c                                                                     c
c*********************************************************************c
c                                                                     c
      subroutine fillarray(array,ndim,val)
      dimension array(ndim)
      do i=1,ndim
         array(i)=val
      enddo
c
      return
      end
c                                                                     c
c*********************************************************************c
c                                                                     c
      subroutine ghtcalc(sigh,pstx,prs,qvp,tmk,ter,ght,rgas,ussalr,
     &      grav,refslp,refslt,reflaps,refstratt,ptop,inhyd,
     &      miy,mjx,mkzh,grid)
c
      dimension sigh(mkzh),pstx(miy,mjx),
     &   qvp(miy,mjx,mkzh),tmk(miy,mjx,mkzh),ter(miy,mjx),
     &   ght(miy,mjx,mkzh),prs(miy,mjx,mkzh),
     &   qvpd(miy,mjx,mkzh),tmkd(miy,mjx,mkzh),prsd(miy,mjx,mkzh)
c
      dimension tv(100)
c
      character*1 grid
c
      expon=rgas*ussalr/grav
      exponi=1./expon
c
      if (inhyd.eq.0) then  ! hydrostatic sigma-coord. form
c
      do 1000 j = 1, mjx-1
      do 1000 i = 1, miy-1
c
c   Calculate tv
c
      do k=1,mkzh

         if (grid.eq.'d') then
         do jj=1,mjx
         do ii=1,miy
            iph=min(ii,miy-1)
            jph=min(jj,mjx-1)
            imh=max(ii-1,1)
            jmh=max(jj-1,1)
            tmkd(ii,jj,k)=.25*(tmk(iph,jph,k)+tmk(imh,jph,k)+
     &                   tmk(iph,jmh,k)+tmk(imh,jmh,k))
c            print *,ii,jj,dter(ii,jj)
c            pause
         enddo
         enddo
         do jj=1,mjx
         do ii=1,miy
            iph=min(ii,miy-1)
            jph=min(jj,mjx-1)
            imh=max(ii-1,1)
            jmh=max(jj-1,1)
            qvpd(ii,jj,k)=.25*(qvp(iph,jph,k)+qvp(imh,jph,k)+
     &                   qvp(iph,jmh,k)+qvp(imh,jmh,k))
c            print *,ii,jj,dter(ii,jj)
c            pause
         enddo
         enddo
         do jj=1,mjx
         do ii=1,miy
            iph=min(ii,miy-1)
            jph=min(jj,mjx-1)
            imh=max(ii-1,1)
            jmh=max(jj-1,1)
            prsd(ii,jj,k)=.25*(prs(iph,jph,k)+prs(imh,jph,k)+
     &                   prs(iph,jmh,k)+prs(imh,jmh,k))
c            print *,ii,jj,dter(ii,jj)
c            pause
         enddo
         enddo
         else
            tmkd(i,j,k)=tmk(i,j,k)
            qvpd(i,j,k)=qvp(i,j,k)
            prsd(i,j,k)=prs(i,j,k)
         endif

         tv(k)=virtual(tmkd(i,j,k),.001*qvpd(i,j,k))
      enddo
c
c   Calculate geopotential height at lowest half sigma level,
c      assuming tv folows standard lapse rate below sigh(mkzh),
c      and using altimeter equation.
c
      psurf=pstx(i,j)+ptop
      ght(i,j,mkzh)=ter(i,j)+tv(mkzh)/ussalr*
     &   ((psurf/prsd(i,j,mkzh))**expon - 1.)
c
      do k = mkzh-1,1,-1
         tvavg=.5*(tv(k)+tv(k+1))
         ght(i,j,k) = ght(i,j,k+1) + (rgas*tvavg/grav)*
     &      log(prsd(i,j,k+1)/prsd(i,j,k))
      enddo
c
 1000 continue
c
      else  ! nonhydrostatic sigma-coord. form
c
      cc1=rgas/grav*(-.5)*reflaps
      cc2=rgas/grav*(reflaps*log(.01*refslp)-refslt)
      cc3=rgas/grav*(refslt-.5*reflaps*log(.01*refslp))*log(.01*refslp)
c
      if (grid.eq.'x') then
         iadd = 0
      else
         iadd = 1
      end if

      alnpreftpause=(refstratt-refslt)/reflaps+log(.01*refslp)
      ztpause=cc1*alnpreftpause*alnpreftpause+
     &   cc2*alnpreftpause+cc3
      do k=1,mkzh
      do j = 1, mjx-1+iadd
      do i = 1, miy-1+iadd
         alnpref=log(sigh(k)*pstx(i,j)+ptop)
         if (alnpref.gt.alnpreftpause) then
            ght(i,j,k)=cc1*alnpref*alnpref+cc2*alnpref+cc3
         else
            ght(i,j,k)=ztpause+rgas*refstratt/grav*
     &         (alnpreftpause-alnpref)
         endif
      enddo
      enddo
      enddo
c
      endif
c
      return
      end
c                                                                     c
c*********************************************************************c
c                                                                     c
      subroutine mconvert(mdate,mhour,idir,nsplityear)
c
c   mdate: an 8-digit integer specification for a date,
c      given as yymmddhh
c   mhour: an integer specificying the number of hours since
c      00 UTC 1 January 1 AD.
c
c   This routine converts an mdate to an mhour if idir=1, or vice versa
c   if idir=-1.
c
c   If idir=1, how do we know what century mdate refers to?  You
c   provide a year, called "nsplityear", denoted "aabb".  If mdate
c   is denoted "yymmddhh", then if yy >or= bb, the century is
c   assumed to be aa.  Otherwise it is assumed to be the century
c   after aa, or aa+1.
c
c   Leap year definition: every fourth year has a 29th day in February,
c      with the exception of century years not divisible by 400.
c
      dimension ndaypmo(12)
      integer yy,mm,dd,hh,aa,bb
      data ndaypmo /31,28,31,30,31,30,31,31,30,31,30,31/
c
      if (idir.eq.1) then
c
      yy=mdate/1000000
      bb=mod(nsplityear,100)
      aa=nsplityear-bb
      iyear=aa+yy
      if (yy.lt.bb) iyear=iyear+100
      iyearp=iyear-1
      idayp = iyearp*365 + iyearp/4 - iyearp/100 +iyearp/400
      mm=mod(mdate,1000000)/10000
      imonthp=mm-1
      if ((mod(iyear,4).eq.0.and.mod(iyear,100).ne.0).or.
     &    mod(iyear,400).eq.0)
     &   ndaypmo(2)=29
      do i=1,imonthp
         idayp=idayp+ndaypmo(i)
      enddo
      ndaypmo(2)=28
      dd=mod(mdate,10000)/100
      idayp=idayp+dd-1
      hh=mod(mdate,100)
      mhour=24*idayp+hh
c
      else
c
      nhour=mhour
c
c   Get an estimate of iyear that is guaranteed to be close to but
c   less than the current year
c
      iyear = max(0,nhour-48)*1.14079e-4
      ihour=24*(iyear*365+iyear/4-iyear/100+iyear/400)
 10   iyear=iyear+1
      ihourp=ihour
      ihour = 24*(iyear*365 + iyear/4 - iyear/100 +iyear/400)
      if (ihour.le.nhour) goto 10
      nhour=nhour-ihourp
      if ((mod(iyear,4).eq.0.and.mod(iyear,100).ne.0).or.
     &    mod(iyear,400).eq.0)
     &   ndaypmo(2)=29
      imo=0
      ihour=0
 20   imo=imo+1
      ihourp=ihour
      ihour=ihour+24*ndaypmo(imo)
      if (ihour.le.nhour) goto 20
      nhour=nhour-ihourp
      ndaypmo(2)=28
      iday = nhour/24 + 1
      ihour=mod(nhour,24)
      mdate=mod(iyear,100)*1000000+imo*10000+iday*100+ihour
c
      endif
c
      return
      end
c                                                                     c
c*********************************************************************c
c                                                                     c
      subroutine skpflds(iunit,mifv1,nskip)
      integer mifv1(1000,20)
      n3d = mifv1(201,mifv1(1,1))
      n2d = mifv1(202,mifv1(1,1))
      do iskp=1,nskip
         do iskpflds = 1, n3d
            read(iunit,end=30)
         enddo
         do iskpflds=n3d+1, n3d+n2d
            read(iunit,end=30)
         enddo
      enddo
   30 continue
      return
      end
c                                                                     c
c*********************************************************************c
c                                                                     c


      subroutine create_cdl(nx,ny,nz,nfld,i1d,i2d,i3d,cdfid,varid,
     >   idz,idcon,idtime,numcon,twod,runnam,nlen,ioflg,fldnm,
     >   ndim,nbase,units,grid,x0,y0,z0,dx,zmax)

c  creates a cdl file, then calls ncgen to create a netcdf
c  file that IVE can read.  For use with mm5 output data
c
      include '/usr/local/netcdf/include/netcdf.inc'    

      integer nx,ny,nz,numcon,i1d,i2d,i3d,nlen
      integer cdfid,varid(nfld),idz(i1d),idcon(numcon)
      integer idtime,length1,length2,flgth1,flgth2,tlgth
      real dx,dy,dz,xmin,ymin,zmin,dy_out,x0,y0,z0
      logical twod
      character*80 cdl_out,cdf_out,command,command2,chtemp,tmp
      character*80 runnam
      character*2 grd,chmake
      character*10 fldnm(nfld)
      
      integer ndim(nfld),nbase(nfld),ioflg(nfld),status

      character*6 units(nfld)
      character*1 grid(nfld)

c      do k = 1,nfld
c        print *,k,fldnm(k),ndim(k),nbase(k),units(k),grid(k)
c      end do

      dx = dx*1000.
      dy = dx
      dz = zmax/real(nz)

      if ((twod).and.(ny.ne.1)) then
        write(6,*) 'Error:  confused about y dimension in set_ive.'
        write(6,*) 'Stopping run.'
        stop
      end if
*
*  If 2d run, then set dy_out to zero so that all fields are
*    unstaggered in y
*
      if (twod) then
        dy_out = 0.
      else
        dy_out = dy
      end if

      grd = 'd1'
      tmp = '_g'//grd(1:2)
      tmp = tmp(1:4)//'.cdl'

      nlen = index(runnam,' ') - 1
      cdl_out = runnam(1:nlen)//tmp(1:8)
      flgth1 = nlen+8
c      print *,nlen,cdl_out,runnam,flgth1
c      pause

      write(6,*) ' USROUT: cdl file is ',cdl_out(1:flgth1)
*
*  first create cdl file; then use ncgen to generate the associated
*    netcdf file
*
      iunit = 11
      open (unit=iunit, file=cdl_out(1:flgth1), status='new')

      write(iunit,*) 'netcdf TestOut{'
      write(iunit,*)
*
* dimensions (here nx,ny,nz refer to dim's of scalar grid)
*
      write(iunit,*) 'dimensions:'
      write(iunit,*) 'nx=',nx,';'
      write(iunit,*) 'ny=',ny,';'
      write(iunit,*) 'nz=',nz,';'
      write(iunit,*) 'nxm1=',nx-1,';'
      if (twod) then
        write(iunit,*) 'nym1=',1,';'
      else
        write(iunit,*) 'nym1=',ny-1,';'
      end if
      write(iunit,*) 'nzp1=',nz+1,';'
      write(iunit,*) 'time=UNLIMITED;'
      write(iunit,*) 'one=',1,';'
      write(iunit,*)
*
* constants (add more constants here if needed; set numcon
*   in usrout3i appropriately)
*
      write(iunit,*) 'variables:'
      write(iunit,*)
      write(iunit,*) 'float time(time);'
      write(iunit,*) 'time:units="h";'
      write(iunit,*)
      write(iunit,*) 'float ptop(one);'
      write(iunit,*) 'ptop:units="Pa";'
      write(iunit,*) 'ptop:def="pressure top";'
      write(iunit,*) 'ptop:no_button=1;'
      write(iunit,*)
      write(iunit,*) 'float ps0(one);'
      write(iunit,*) 'ps0:units="Pa";'
      write(iunit,*) 'ps0:def="reference pressure";'
      write(iunit,*) 'ps0:no_button=1;'
      write(iunit,*)
      write(iunit,*) 'float ts0(one);'
      write(iunit,*) 'ts0:units="K";'
      write(iunit,*) 'ts0:def="reference temp";'
      write(iunit,*) 'ts0:no_button=1;'
      write(iunit,*)
      write(iunit,*) 'float tlp(one);'
      write(iunit,*) 'tlp:units="K/Pa";'
      write(iunit,*) 'tlp:def="reference lpase rate";'
      write(iunit,*) 'tlp:no_button=1;'
      write(iunit,*)

*
* 2d variables
*
      write(iunit,*) 'float fsigma(nzp1);'
      write(iunit,*) 'fsigma:units="";'
      write(iunit,*) 'fsigma:def="full sigma levels";'
      write(iunit,*) 'fsigma:no_button=1;'
      write(iunit,*)
      write(iunit,*) 'float hsigma(nz);'
      write(iunit,*) 'hsigma:units="";'
      write(iunit,*) 'hsigma:def="half sigma levels";'
      write(iunit,*) 'hsigma:no_button=1;'
      write(iunit,*) 
      write(iunit,*) 'float xter(nym1,nxm1);'
      write(iunit,*) 'xter:units="m";'
      write(iunit,*) 'xter:def="terrain at cross points";'
      write(iunit,*) 'xter:xmin=',x0+(dx/2.),';'
      write(iunit,*) 'xter:ymin=',y0+(dy/2.),';'
      write(iunit,*) 'xter:xmax=',x0 + (nx-2)*dx,';'
      write(iunit,*) 'xter:ymax=',y0 + (ny-2)*dy_out,';'
      write(iunit,*) 'xter:no_button=1;'
      write(iunit,*) 
      write(iunit,*) 'float dter(ny,nx);'
      write(iunit,*) 'dter:units="m";'
      write(iunit,*) 'dter:def="terrain at dot points";'
      write(iunit,*) 'dter:xmin=',x0,';'
      write(iunit,*) 'dter:ymin=',y0,';'
      write(iunit,*) 'dter:xmax=',x0 + (nx-1)*dx,';'
      write(iunit,*) 'dter:ymax=',y0 + (ny-1)*dy_out,';'
      write(iunit,*) 'dter:no_button=1;'
      write(iunit,*) 
      write(iunit,*) 'float xlus(ny,nx);'
      write(iunit,*) 'xlus:units="m";'
      write(iunit,*) 'xlus:def="terrain at dot points";'
      write(iunit,*) 'xlus:xmin=',x0,';'
      write(iunit,*) 'xlus:ymin=',y0,';'
      write(iunit,*) 'xlus:xmax=',x0 + (nx-1)*dx,';'
      write(iunit,*) 'xlus:ymax=',y0 + (ny-1)*dy_out,';'
      write(iunit,*) 'xlus:no_button=1;'
      write(iunit,*) 
      write(iunit,*) 'float pstx(nym1,nxm1);'
      write(iunit,*) 'pstx:units="Pa";'
      write(iunit,*) 'pstx:def="perturbation pres at cross points";'
      write(iunit,*) 'pstx:xmin=',x0+(dx/2.),';'
      write(iunit,*) 'pstx:ymin=',y0+(dy/2.),';'
      write(iunit,*) 'pstx:xmax=',x0 + (nx-2)*dx,';'
      write(iunit,*) 'pstx:ymax=',y0 + (ny-2)*dy_out,';'
      write(iunit,*) 'pstx:no_button=1;'
      write(iunit,*)
      write(iunit,*) 'float pstd(ny,nx);'
      write(iunit,*) 'pstd:units="Pa";'
      write(iunit,*) 'pstd:def="perturbation pres at dot points";'
      write(iunit,*) 'pstd:xmin=',x0,';'
      write(iunit,*) 'pstd:ymin=',y0,';'
      write(iunit,*) 'pstd:xmax=',x0 + (nx-1)*dx,';'
      write(iunit,*) 'pstd:ymax=',y0 + (ny-1)*dy_out,';'
      write(iunit,*) 'pstd:no_button=1;'
      write(iunit,*) 
      write(iunit,*) 'float xlat(nym1,nxm1);'
      write(iunit,*) 'xlat:units="degree";'
      write(iunit,*) 'xlat:def="latitide at cross points";'
      write(iunit,*) 'xlat:xmin=',x0+(dx/2.),';'
      write(iunit,*) 'xlat:ymin=',y0+(dy/2.),';'
      write(iunit,*) 'xlat:xmax=',x0 + (nx-2)*dx,';'
      write(iunit,*) 'xlat:ymax=',y0 + (ny-2)*dy_out,';'
      write(iunit,*) 'xlat:no_button=1;'
      write(iunit,*)
      write(iunit,*) 'float xlon(nym1,nxm1);'
      write(iunit,*) 'xlon:units="degree";'
      write(iunit,*) 'xlon:def="longitude at cross points";'
      write(iunit,*) 'xlon:xmin=',x0+(dx/2.),';'
      write(iunit,*) 'xlon:ymin=',y0+(dy/2.),';'
      write(iunit,*) 'xlon:xmax=',x0 + (nx-2)*dx,';'
      write(iunit,*) 'xlon:ymax=',y0 + (ny-2)*dy_out,';'
      write(iunit,*) 'xlon:no_button=1;'
      write(iunit,*)
      write(iunit,*) 'float xcor(nym1,nxm1);'
      write(iunit,*) 'xcor:units="1/s";'
      write(iunit,*) 'xcor:def="coriolis at cross points";'
      write(iunit,*) 'xcor:xmin=',x0+(dx/2.),';'
      write(iunit,*) 'xcor:ymin=',y0+(dy/2.),';'
      write(iunit,*) 'xcor:xmax=',x0 + (nx-2)*dx,';'
      write(iunit,*) 'xcor:ymax=',y0 + (ny-2)*dy_out,';'
      write(iunit,*) 'xcor:no_button=1;'
      write(iunit,*)
      write(iunit,*) 'float dcor(ny,nx);'
      write(iunit,*) 'dcor:units="1/s";'
      write(iunit,*) 'dcor:def="coriolis at dot points";'
      write(iunit,*) 'dcor:xmin=',x0,';'
      write(iunit,*) 'dcor:ymin=',y0,';'
      write(iunit,*) 'dcor:xmax=',x0 + (nx-1)*dx,';'
      write(iunit,*) 'dcor:ymax=',y0 + (ny-1)*dy_out,';'
      write(iunit,*) 'dcor:no_button=1;'
      write(iunit,*)
      write(iunit,*) 'float xght(one,nz,nym1,nxm1);'
      write(iunit,*) 'xght:units="m";'
      write(iunit,*) 'xght:def="height of cross points";'
      write(iunit,*) 'xght:xmin=',x0+(dx/2.),';'
      write(iunit,*) 'xght:ymin=',y0+(dy/2.),';'
      write(iunit,*) 'xght:xmax=',x0 + (nx-2)*dx,';'
      write(iunit,*) 'xght:ymax=',y0 + (ny-2)*dy_out,';'
      write(iunit,*) 'xght:zmin=',z0,';'
      write(iunit,*) 'xght:zmax=',zmax,';'
      write(iunit,*) 'xght:no_button=1;'
      write(iunit,*)
      write(iunit,*) 'float dght(one,nz,ny,nx);'
      write(iunit,*) 'dght:units="m";'
      write(iunit,*) 'dght:def="height of dot points";'
      write(iunit,*) 'dght:xmin=',x0,';'
      write(iunit,*) 'dght:ymin=',y0,';'
      write(iunit,*) 'dght:xmax=',x0 + (nx-1)*dx,';'
      write(iunit,*) 'dght:ymax=',y0 + (ny-1)*dy_out,';'
      write(iunit,*) 'dght:zmin=',z0,';'
      write(iunit,*) 'dght:zmax=',zmax,';'
      write(iunit,*) 'dght:no_button=1;'
      write(iunit,*)
c      write(iunit,*) 'float xmap(nym1,nxm1);'
c      write(iunit,*) 'xmap:units="m";'
c      write(iunit,*) 'xmap:def="map factors - cross points";'
c      write(iunit,*) 'xmap:xmin=',x0+(dx/2.),';'
c      write(iunit,*) 'xmap:ymin=',y0+(dy/2.),';'
c      write(iunit,*) 'xmap:xmax=',x0 + (nx-2)*dx,';'
c      write(iunit,*) 'xmap:ymax=',y0 + (ny-2)*dy_out,';'
c      write(iunit,*) 'xmap:no_button=1;'
c      write(iunit,*)
c      write(iunit,*) 'float dmap(ny,nx);'
c      write(iunit,*) 'dmap:units="m";'
c      write(iunit,*) 'dmap:def="map factors - dot points";'
c      write(iunit,*) 'dmap:xmin=',x0,';'
c      write(iunit,*) 'dmap:ymin=',y0,';'
c      write(iunit,*) 'dmap:xmax=',x0 + (nx-1)*dx,';'
c      write(iunit,*) 'dmap:ymax=',y0 + (ny-1)*dy_out,';'
c      write(iunit,*) 'dmap:no_button=1;'
c      write(iunit,*)
c      write(iunit,*) 'float rtmp(ny,nx);'
c      write(iunit,*) 'rtmp:units="K";'
c      write(iunit,*) 'rtmp:def="reservoir temp";'
c      write(iunit,*) 'rtmp:xmin=',x0+(dx/2.),';'
c      write(iunit,*) 'rtmp:ymin=',y0+(dy/2.),';'
c      write(iunit,*) 'rtmp:xmax=',x0 + (nx-2)*dx,';'
c      write(iunit,*) 'rtmp:ymax=',y0 + (ny-2)*dy_out,';'
c      write(iunit,*) 'rtmp:no_button=1;'
c      write(iunit,*)
*
* 3d variables
*
      do k = 1,nfld

        if ((nbase(k).eq.1).or.(ioflg(k).eq.0)) goto 22        

        length1 = index(fldnm(k),' ') - 1

        if (ndim(k).eq.2) then
           if (nbase(k).eq.2) then
              if (grid(k).eq.'x') then
                 write(iunit,*) 'float ',fldnm(k)(1:length1),
     >                      '(one,one,nym1,nxm1);'
              else
                 write(iunit,*) 'float ',fldnm(k)(1:length1),
     >                      '(one,one,ny,nx);'
              end if
           else
              if (grid(k).eq.'x') then
                 write(iunit,*) 'float ',fldnm(k)(1:length1),
     >                      '(time,one,nym1,nxm1);'
              else
                 write(iunit,*) 'float ',fldnm(k)(1:length1),
     >                      '(time,one,ny,nx);'
              end if
           end if

        else if (ndim(k).eq.3) then

           if (nbase(k).eq.2) then
              if (grid(k).eq.'x') then
                 write(iunit,*) 'float ',fldnm(k)(1:length1),
     >                      '(one,nz,nym1,nxm1);'
              else
                 write(iunit,*) 'float ',fldnm(k)(1:length1),
     >                      '(one,nz,ny,nx);'
              end if
           else
              if (grid(k).eq.'x') then
                 write(iunit,*) 'float ',fldnm(k)(1:length1),
     >                      '(time,nz,nym1,nxm1);'
              else
                 write(iunit,*) 'float ',fldnm(k)(1:length1),
     >                      '(time,nz,ny,nx);'
              end if
           end if

        end if 

        length2 = index(units(k),' ') - 1
        if (length2.eq.0) length2 = 1
        if (length2.eq.-1) length2 = len(units(k))
        write(iunit,*) fldnm(k)(1:length1),':units="',
     >                 units(k)(1:length2),'";'
        write(iunit,*) fldnm(k)(1:length1),':display_units="',
     >                 units(k)(1:length2),'";'
*
* set staggering 
*
        if (grid(k).eq.'x') then
           xmin = x0 + (dx/2.)
           ymin = y0 + (dy/2.)
           xmax = x0 + (nx-2)*dx
           ymax = y0 + (ny-2)*dy_out
           if (fldnm(k)(1:length1).eq.'wwf') then
              zmin = 0.
              zmaxn = zmax
           else
              zmin = z0
              zmaxn = zmax
           end if
        else
           xmin = x0
           ymin = y0
           xmax = x0 + (nx-1)*dx
           ymax = y0 + (ny-1)*dy_out
           zmin = z0
           zmaxn = zmax
        end if                
        write(iunit,*) fldnm(k)(1:length1),':xmin=',
     >                        xmin,';'
        write(iunit,*) fldnm(k)(1:length1),':ymin=',
     >                        ymin,';'
        write(iunit,*) fldnm(k)(1:length1),':xmax=',
     >                        xmax,';'
        write(iunit,*) fldnm(k)(1:length1),':ymax=',
     >                        ymax,';'
        if (ndim(k).eq.3) write(iunit,*) fldnm(k)(1:length1),
     >                        ':zmin=',zmin,';'
        if (ndim(k).eq.3) write(iunit,*) fldnm(k)(1:length1),
     >                        ':zmax=',zmaxn,';'
*
* hide IVE button for basic state fields
*
        if (nbase(k).ge.1) then
           write(iunit,*) fldnm(k)(1:length1),':no_button=1;'
        end if
        if (fldnm(k)(1:length1).eq.'wwf') then
           write(iunit,*) fldnm(k)(1:length1),':no_button=1;'
        end if

        write(iunit,*)
 
 22   end do

*
* global attributes
*
      write(iunit,*) '//global attributes:'
      write(iunit,*)

      write(iunit,*) ':domxmin=',x0,';'
      write(iunit,*) ':domxmax=',x0 + (nx-1)*dx,';'
      write(iunit,*) ':x_delta=',dx,';'
      write(iunit,*) ':x_units="m";'
      write(iunit,*) ':x_label="x";'
      write(iunit,*) ':x_display_units="km";'
      write(iunit,*)

      write(iunit,*) ':domymin=',y0,';'
      write(iunit,*) ':domymax=',y0 + (ny-1)*dy_out,';'
      write(iunit,*) ':y_delta=',dy,';'
      write(iunit,*) ':y_units="m";'
      write(iunit,*) ':y_label="y";'
      write(iunit,*) ':y_display_units="km";'
      write(iunit,*)

      write(iunit,*) ':domzmin=',0.,';'
      write(iunit,*) ':domzmax=',zmax,';'
      write(iunit,*) ':z_delta=',dz,';'
      write(iunit,*) ':z_units="m";'
      write(iunit,*) ':z_label="z";'
      write(iunit,*) ':z_display_units="km";'
      write(iunit,*)
*
* miscellaneous comments
*
      write(iunit,*) ':runname="',runnam(1:nlen),'";'
      write(iunit,*)
      write(iunit,*) '}'

      close(iunit)
  
c      call retunit(iunit)
*
*  cdl file done.  Get netcdf file name and make file.
*
      tmp = '_g'//grd(1:2)
      tmp = tmp(1:4)//'.cdf'
      cdf_out = runnam(1:nlen)//tmp(1:8)
      tlgth = 4

      flgth2 = index(cdf_out,' ') - 1
      command(1:len(command)) = ' '
      write(6,*) ' USROUT: netcdf file is ',cdf_out(1:flgth2)
      write(command,*) '/usr/local/netcdf/bin/ncgen -o '
     >     ,cdf_out(1:flgth2),' ',cdl_out(1:flgth1)
      print *,command
      call system(command)
c      write(command2,*) 'rm -f ',cdl_out(1:flgth1)
c      call system(command2)
c
*  get ID's for file and variables.
*
      status = NF_OPEN(cdf_out(1:flgth2),NF_WRITE,cdfid)
      if (status.ne.NF_NOERR) call handle_err(status)
      in2d = 0
      in3d = 0
      print *,"getting nc ids"
      do 44 k = 1,nfld
        if (ioflg(k).eq.0) goto 44
        length1 = index(fldnm(k),' ') - 1
        if (length1.eq.-1) length1 = len(fldnm(k))
    
        status = NF_INQ_VARID(cdfid,fldnm(k)(1:length1),varid(k))
        if (status.ne.NF_NOERR) call handle_err(status)
        print *,fldnm(k),k," ",varid(k)
 44   continue
      status = NF_INQ_VARID(cdfid,'time',idtime)
      if (status.ne.NF_NOERR) call handle_err(status)
      status = NF_INQ_VARID(cdfid,'fsigma',idz(1))
      if (status.ne.NF_NOERR) call handle_err(status)
      status = NF_INQ_VARID(cdfid,'hsigma',idz(2))
      if (status.ne.NF_NOERR) call handle_err(status)
*
* get ID's for scalar constants
*
      status = NF_INQ_VARID(cdfid,'ptop',idcon(1))
      if (status.ne.NF_NOERR) call handle_err(status)
      status = NF_INQ_VARID(cdfid,'ps0',idcon(2))
      if (status.ne.NF_NOERR) call handle_err(status)
      status = NF_INQ_VARID(cdfid,'ts0',idcon(3))
      if (status.ne.NF_NOERR) call handle_err(status)
      status = NF_INQ_VARID(cdfid,'tlp',idcon(4))
      if (status.ne.NF_NOERR) call handle_err(status)

      return
      end


*------------------------------------------------------------------------

      subroutine write3d(cdfid,varid,ist,iln,var,ny,nx,sf)

      include '/usr/local/netcdf/include/netcdf.inc'    

      integer cdfid,varid,ny,nx
      integer ist(4),iln(4)
      real var(ny,nx,iln(3)),sf
      real varout(iln(1),iln(2),iln(3))

      integer status
      
*
*
      print *,ist
      print *,iln
      do k = 1,iln(3)
         do j = 1,iln(2)
            do i = 1,iln(1)
               if(var(j,i,iln(3)-k+1) .gt. 1.e+25) 
     &     var(j,i,iln(3)-k+1)= 1.e+25
               if(var(j,i,iln(3)-k+1) .lt.-1.e+25) 
     &     var(j,i,iln(3)-k+1)=-1.e+25
               if(abs(var(j,i,iln(3)-k+1)).lt. 1.e-25) 
     &     var(j,i,iln(3)-k+1)= 0.
               varout(i,j,k) = var(j,i,iln(3)-k+1) * sf
            enddo
         end do
      enddo
*
* make sure we send four-byte integers to cdf output routines
*

      status = NF_PUT_VARA_REAL(cdfid,varid,ist,iln,varout)
      if (status.ne.NF_NOERR) call handle_err(status)

      return
      end
*------------------------------------------------------------------------

      subroutine write2d(cdfid,varid,ist,iln,var,ny,nx,sf)

      include '/usr/local/netcdf/include/netcdf.inc'    

      integer cdfid,varid,ny,nx
      integer ist(4),iln(4)
      real var(ny,nx),sf
      real varout(iln(1),iln(2))

      integer status
*
*
c      print *,'2d',ist
c      print *,'2d',iln
c      print *,"write2d ",varid
      do i = 1,iln(1)
         do j = 1,iln(2)
           if(var(j,i) .gt. 1.e+25) var(j,i)= 1.e+25
           if(var(j,i) .lt.-1.e+25) var(j,i)=-1.e+25
           if(abs(var(j,i)).lt. 1.e-25) var(j,i)= 0.
            varout(i,j) = var(j,i) * sf
c            if (varid.eq.9) then 
c               print *,i,j,varid,varout(i,j)
c            end if
         enddo
      enddo
*
* make sure we send four-byte integers to cdf output routines
*

      status = NF_PUT_VARA_REAL(cdfid,varid,ist,iln,varout)
      if (status.ne.NF_NOERR) call handle_err(status)

      return
      end

*-----------------------------------------------------------------------

      subroutine clsfile(cdfid)

      include '/usr/local/netcdf/include/netcdf.inc'    
      integer cdfid, status
      
      status = NF_CLOSE(cdfid)
      if (status.ne.NF_NOERR) call handle_err(status)

      return
      end
c
c *********************************************************************
c

      subroutine write2d_base(cdfid,varid,ist,iln,var,ny,nx,sf)

      include '/usr/local/netcdf/include/netcdf.inc'    

      integer nx,ny,cdfid,varid
      integer ist(2),iln(2),status
      real var(ny,nx),sf
      real varout(iln(1),iln(2))

      character*5 name

c      print *,"write2d_base ",varid
      do i = 1,iln(1)
        do j = 1,iln(2)
           if(var(j,i) .gt. 1.e+25) var(j,i)= 1.e+25
           if(var(j,i) .lt.-1.e+25) var(j,i)=-1.e+25
           if(abs(var(j,i)).lt. 1.e-25) var(j,i)= 0.
           varout(i,j) = var(j,i) * sf
c             print *,i,j,varid,varout(i,j)
        end do
c        pause
      enddo
*
* send four-byte integers to cdf output routines
*
      status = NF_PUT_VARA_REAL(cdfid,varid,ist,iln,varout)
      if (status.ne.NF_NOERR) call handle_err(status)

      return 
      end

*--------------------------------------------------------------------------

      subroutine setfld(ndim,grid,i4s,i4l,itime,miy,mjx,mkzh)

      integer ndim,itime,i4s(4),i4l(4),miy,mjx,mkzh
    
      character*1 grid

      if (ndim.eq.2) then
         if (grid.eq.'x') then
            i4s(1) = 1
            i4s(2) = 1
            i4s(3) = 1
            i4s(4) = itime+1
            i4l(1) = mjx-1
            i4l(2) = miy-1
            i4l(3) = 1
            i4l(4) = 1
          else
            i4s(1) = 1
            i4s(2) = 1
            i4s(3) = 1
            i4s(4) = itime+1
            i4l(1) = mjx
            i4l(2) = miy
            i4l(3) = 1
            i4l(4) = 1
          end if
      else
         if (grid.eq.'x') then
            i4s(1) = 1
            i4s(2) = 1
            i4s(3) = 1
            i4s(4) = itime+1
            i4l(1) = mjx-1
            i4l(2) = miy-1
            i4l(3) = mkzh
            i4l(4) = 1
          else
            i4s(1) = 1
            i4s(2) = 1
            i4s(3) = 1
            i4s(4) = itime+1
            i4l(1) = mjx
            i4l(2) = miy
            i4l(3) = mkzh
            i4l(4) = 1
          end if
      end if

      return
      end

C
C **********************************************************************
C
      subroutine handle_err(status)
   
      include '/usr/local/netcdf/include/netcdf.inc'    
   
      integer status

      if (status.ne.NF_NOERR) then
         write(6,*) 'netcdf error: ',NF_STRERROR(status)
         stop ' program stopped'
      end if

      end
c                                                                     c
c*********************************************************************c
c                                                                     c
      subroutine jumpendfile(iunit,ijump)
      if (ijump.le.0) return
      isofar=0
   10 continue
   20 continue
      read(iunit,end=100)
      goto 20
  100 continue
      isofar=isofar+1
      if (isofar.lt.ijump) goto 10
      return
      end
c                                                                     c
c*********************************************************************c
c                                                                     c
      subroutine getobs(iobs,iobsfnd,mdate,iunit)
c
      if (iobs.eq.1) then
  280    read(iunit,end=285) mdategp
  283    goto 286
  285    print*,'   Couldn''t find matching obs dataset.'
         iobs=0
         iobsfnd=0
         goto 287
  286    if (mdategp.gt.mdate) then
            backspace (iunit)
            iobsfnd=0
            print*,'   Model output time is earlier than'
            print*,'      first obs data time.'
         elseif (mdategp.lt.mdate) then
            read(iunit)
            goto 280
         else
            iobsfnd=1
            print*,'   Found matching time in obs dataset.'
         endif
      endif
  287 continue
      return
      end
c                                                                     c
c*********************************************************************c
c                                                                     c
      function virtual(temp,ratmix)
c
c   This function returns virtual temperature in K, given temperature
c      in K and mixing ratio in kg/kg.
c
      eps=0.622
      virtual=temp*(eps+ratmix)/(eps*(1.+ratmix))
      return
      end
