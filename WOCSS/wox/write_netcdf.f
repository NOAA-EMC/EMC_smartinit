      subroutine  write_netcdf(infile,minjul,ncall,woxfile,woxfilep)

      include 'netcdf.inc'

      include 'ngrids.param'
c
      include 'anchor.incl'
      include 'flower.incl'
      include 'limits.incl'
      include 'staloc.incl'
      include 'tsonds.incl'
c
      character*16 cdffile
      character*13 infile,outtime,outtimep1
      character*1 cinv
      character*4 strh
      character*16 woxfile,woxfilep

      integer*4 cdfid,status,varid,i4s(4),i4l(4),i,j,i2s(2),i2l(2)
      integer*4 itout,iwmag,iwdir,itime
      real*4 uw(nxgrd,nygrd,1),vw(nxgrd,nygrd,1),ww(nxgrd,nygrd,1)
      real*4 t(nxgrd,nygrd,1),p(nxgrd,nygrd,1)
      real*4 tout,wmag,wdir

      data i2s /1,1/
      data i2l /1,1/
      data i4s /1,1,1,1/
      data i4l /nxgrd,nygrd,1,1/

      cinv = "1"

      print *,'Writing out netcdf file'

c
c  open netcdf template file

      cdffile = woxfile(1:8)//'_'//woxfile(9:12)//'.nc'
      print *, 'Creating file: ',cdffile
      print *, 'cdfid: ', cdfid
      status = NF_OPEN(cdffile,NF_WRITE,cdfid)
      print *, 'NF_NOERR,status,cdfid: ', NF_NOERR,status,cdfid
      if (status.ne.NF_NOERR) call handle_err(status)
      print *, 'After File Open'

c
c  write u field to netcdf file

      do i = 1,nxgrd
         do j = 1,nygrd
            uw(i,j,1) = float(iugraf(i,j,1))/100.
            if (abs(uw(i,j,1)).gt.200.) uw(i,j,1) = -99999.
         end do
      end do

      status = NF_INQ_VARID(cdfid,'uw',varid)
      if (status.ne.NF_NOERR) call handle_err(status)

      status = NF_PUT_VARA_REAL(cdfid,varid,i4s,i4l,uw)
      if (status.ne.NF_NOERR) call handle_err(status)

      status = NF_INQ_VARID(cdfid,'uwInventory',varid)
      if (status.ne.NF_NOERR) call handle_err(status)

      status = NF_PUT_VARA_TEXT(cdfid,varid,i2s,i2l,cinv)
      if (status.ne.NF_NOERR) call handle_err(status)

c
c  write v field to netcdf file

      do i = 1,nxgrd
         do j = 1,nygrd
            vw(i,j,1) = float(ivgraf(i,j,1))/100.
            if (abs(vw(i,j,1)).gt.200.) vw(i,j,1) = -99999.
         end do
      end do

      status = NF_INQ_VARID(cdfid,'vw',varid)
      if (status.ne.NF_NOERR) call handle_err(status)

      status = NF_PUT_VARA_REAL(cdfid,varid,i4s,i4l,vw)
      if (status.ne.NF_NOERR) call handle_err(status)

      status = NF_INQ_VARID(cdfid,'vwInventory',varid)
      if (status.ne.NF_NOERR) call handle_err(status)

      status = NF_PUT_VARA_TEXT(cdfid,varid,i2s,i2l,cinv)
      if (status.ne.NF_NOERR) call handle_err(status)

c
c  write temp field to netcdf file
c  first convert potential temp to temperature

      do i = 1,nxgrd
         do j = 1,nygrd
            p(i,j,1) = float(iprgrf(i,j,1))*10.
            if ((p(i,j,1).lt.0.).or.(p(i,j,1).gt.110000.)) then 
                p(i,j,1) = -99999.
            end if
         end do
      end do

      do i = 1,nxgrd
         do j = 1,nygrd
            t(i,j,1) = (float(iptgrf(i,j,1))/10.)*
     +              ((p(i,j,1)/100000.)**(287./1004.))
            if ((t(i,j,1).lt.100.).or.(t(i,j,1).gt.400.)) then 
                p(i,j,1) = -99999.
            end if
         end do
      end do

      status = NF_INQ_VARID(cdfid,'t',varid)
      if (status.ne.NF_NOERR) call handle_err(status)

      status = NF_PUT_VARA_REAL(cdfid,varid,i4s,i4l,t)
      if (status.ne.NF_NOERR) call handle_err(status)


      status = NF_INQ_VARID(cdfid,'tInventory',varid)
      if (status.ne.NF_NOERR) call handle_err(status)

      status = NF_PUT_VARA_TEXT(cdfid,varid,i2s,i2l,cinv)
      if (status.ne.NF_NOERR) call handle_err(status)

c
c  write pressure field to netcdf file

      status = NF_INQ_VARID(cdfid,'p',varid)
      if (status.ne.NF_NOERR) call handle_err(status)

      status = NF_PUT_VARA_REAL(cdfid,varid,i4s,i4l,p)
      if (status.ne.NF_NOERR) call handle_err(status)

      status = NF_INQ_VARID(cdfid,'pInventory',varid)
      if (status.ne.NF_NOERR) call handle_err(status)

      status = NF_PUT_VARA_TEXT(cdfid,varid,i2s,i2l,cinv)
      if (status.ne.NF_NOERR) call handle_err(status)

c
c  write topo field to netcdf file

      status = NF_INQ_VARID(cdfid,'topo',varid)
      if (status.ne.NF_NOERR) call handle_err(status)

      status = NF_PUT_VARA_REAL(cdfid,varid,i4s,i4l,sfcht)
      if (status.ne.NF_NOERR) call handle_err(status)

c
c close netcdf file

      status = NF_CLOSE(cdfid)
      if (status.ne.NF_NOERR) call handle_err(status)

      print *,'Finished writing to netcdf file'

C  write ascii files next

      open(5,file=woxfile(5:12)//'_wox_tmp.out',form='formatted', 
     $       status='unknown')

      outtime = woxfile(1:8)//'_'//woxfile(9:12)
      outtimep1 = woxfilep(1:8)//'_'//woxfilep(9:12)

      print *,outtime,' ',outtimep1

      write(5,*) 'ASCIIGRID'
      write(5,*) 'SCALAR'
      write(5,*) 'T'
      write(5,*) 'MSO'
      write(5,*) '<notype>'
      write(5,*) 'WOCSS'
      write(5,*) '00000000_0000'
      write(5,*) 'Grid211'
      write(5,*) '257 257 29 39 8 8'
      write(5,*) 'F'
      write(5,*) 'Surface Temp'
      write(5,*) '-50 130 0 0'
      write(5,*) '0 3600 3600'
      write(5,120),outtime,outtimep1

 120  format(1x,a13,1x,a13)
 110  format(i3)

      do j = 1,nxgrd
         do i = 1,nygrd
            tout = (t(i,j,1) - 273.16)
            itout = nint(tout*(9./5.)+32.)
            write(5,110) itout
         end do
      end do
      
      close(5)

      open(6,file=woxfile(5:12)//'_wox_wnd.out',form='formatted', 
     $       status='unknown')

      write(6,*) 'ASCIIGRID'
      write(6,*) 'VECTOR'
      write(6,*) 'Wind'
      write(6,*) 'MSO'
      write(6,*) '<notype>'
      write(6,*) 'WOCSS'
      write(6,*) '00000000_0000'
      write(6,*) 'Grid211'
      write(6,*) '257 257 29 39 8 8'
      write(6,*) 'Knots'
      write(6,*) 'Surface Wind'
      write(6,*) '0 200 0 0'
      write(6,*) '0 3600 3600'
      write(6,120),outtime,outtimep1

 115  format(i3,1x,i3)

      do j = 1,nxgrd
         do i = 1,nygrd
            wmag = ((uw(i,j,1)**2.+vw(i,j,1)**2.)**0.5) * 1.94
            if (wmag.gt.125) wmag = 125.

            if (vw(i,j,1).eq.0.) then
               if (uw(i,j,1).lt.0.) wdir = 90.
               if (uw(i,j,1).gt.0.) wdir = 270.
            else 
               wtan = uw(i,j,1)/vw(i,j,1)
               if (vw(i,j,1).gt.0.) then
                  wdir = (atan(wtan) * 57.29577951) + 180.
               else
                  wdir = (atan(wtan) * 57.29577951) + 360.
               end if
               if (wdir.ge.360) wdir = wdir - 360.
            end if
            iwmag = nint(wmag)
            iwdir = nint(wdir)
   
            write(6,115) iwmag,iwdir
         end do
      end do
      
      close(6)

      return

      end
C
C **********************************************************************
C
      subroutine handle_err(status)

      include 'netcdf.inc'
   
      integer*4 status

      if (status.ne.NF_NOERR) then
         write(6,*) 'netcdf error: ',NF_STRERROR(status)
         stop ' program stopped'
      end if

      end
c                                                                     c
c*********************************************************************c
