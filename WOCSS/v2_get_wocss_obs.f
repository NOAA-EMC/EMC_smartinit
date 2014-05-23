      program conv3
*
* The original code was graciously given by
* Mark Albright at the University of Washington.
* In its original form, the code was designed to
* create a large ASCII text file which was used
* as input for the GEMPAK "gdedit" program. The
* latter program converted the text file to a
* GEMPAK Data Management File format (binary).
*
* This new version bypasses creation of the text
* file (which can be hundreds of Megabytes at least)
* and, instead, converts from MM5V2 IEEE output files
* directly to the GEMPAK Data Management File format.
*
* Version 2 interpolates values from Sigma to Isobaric
* vertical coordinates.
*
* Version 3 computes cloud water on isobaric
* vertical coordinates from Psfc to Ptop.
*
* [D. Miller, Naval Postgraduate School, 30 MAY 97]
*
*
      parameter(ixl=140,jyl=140,kzl=50)
      parameter(nwsite=9,nzn=20)
      integer mif(1000,20)
      real mrf(1000,20)
      integer iyr, ihr, ilvl, ii, jj, kk, aa,bb,cc,dd,i,j,k
      integer ftim, kzs, kze, ix, jy, kz, kzg, iwrite
      character innam*80, outnam*80, tmpa*40, cyr*6
      character fdatim*20
      character vcord*12, parm*12, curryr*4, curyr*4
      character vcorda(50)*12, parma(50)*12
      character*80 mifc(1000,20), mrfc(1000,20)
      character*72 tproj, tgarea, tkxky
      character  field*8
      dimension xcm(ixl,jyl,8), x3dm(ixl,jyl,kzl,7)
      dimension uu(ixl,jyl,kzl), vv(ixl,jyl,kzl), sih(kzl)
      real tt(ixl,jyl,kzl), qv(ixl,jyl,kzl), tke(ixl,jyl,kzl),
*&&&      real tt(ixl,jyl,kzl), qv(ixl,jyl,kzl),
     +qc(ixl,jyl,kzl), qr(ixl,jyl,kzl), ww(ixl,jyl,kzl+1),
     +pp(ixl,jyl,kzl), rh(ixl,jyl,kzl), rtt(ixl,jyl,kzl),
     +ght(ixl,jyl,kzl),
     +junk3(ixl,jyl,kzl), rhd(ixl,jyl,kzl), wwd(ixl,jyl,kzl)
      real pstx(ixl,jyl), tg(ixl,jyl), rc(ixl,jyl), re(ixl,jyl),
     + ter(ixl,jyl), junk2(ixl,jyl),
     +xmap(ixl,jyl), dotmap(ixl,jyl), dotcor(ixl,jyl), irst(ixl,jyl),
     +xlat(ixl,jyl), xlon(ixl,jyl), xluse(ixl,jyl), xsnowc(ixl,jyl),
     +pstd(ixl,jyl), xcor(ixl, jyl), dew(ixl,jyl,kzl), sfp(ixl,jyl),
     +slp(ixl,jyl), rl(ixl, jyl), sshf(ixl,jyl), pblr(ixl,jyl),
     +pblh(ixl,jyl), ml(ixl, jyl), slhf(ixl,jyl), sfpym(ixl,jyl),
     +sfpxm(ixl,jyl), nsrf(ixl,jyl), pr(ixl,jyl,kzl),
     +vqcl(ixl,jyl), vqcm(ixl,jyl), vqch(ixl,jyl)
      real d3d(ixl,jyl,kzl), d2d(ixl,jyl), d1d(ixl), d0d,
     +graupel(ixl,jyl,kzl), nci(ixl,jyl,kzl)
      real wmixrat,tvll, beta, gpll, tvavg, q, t, p, e
      real ttd(ixl,jyl,kzl), dewd(ixl,jyl,kzl), sfpd(ixl,jyl),
     +slpd(ixl,jyl), cice(ixl,jyl,kzl), sice(ixl,jyl,kzl),
     +tgd(ixl,jyl), terd(ixl,jyl), ghtd(ixl,jyl,kzl),
     +clw(ixl,jyl,kzl), rnw(ixl,jyl,kzl), dtdz(ixl,jyl,kzl),
     +xlow(ixl,jyl), xmid(ixl,jyl), xhi(ixl,jyl), utot(ixl,jyl),
     +flow(100), fmid(100), fhi(100), prd(ixl,jyl,kzl),
     +plow(100), pmid(100), phi(100)
      real qctot(ixl,jyl), qrtot(ixl,jyl), qvtot(ixl,jyl)
      real refrat, yllc, yurc, xllc, xurc, xlatll, xlatur,
     +xlonll, xlonur
      integer iahr, ibhr, output_hr, oahr, filnum, ftime, mdate,
     +domid, fhour
      integer ilvla(kzl), iyr1(10)
      real tot_prec(ixl,jyl), prec_3hr(ixl,jyl), rib(ixl,jyl),  
     +riad(ixl,jyl), ribd(ixl,jyl), rcb(ixl,jyl), 
     +reb(ixl,jyl), oahred(ixl,jyl), oahrcd(ixl,jyl),
     +last_output_hr_tot_prec(ixl,jyl), prcn_3hr(ixl,jyl)
      real vcoord(100), pmb(kzl), a(kzl)
*
* WOCSS arrays
*
      integer idop(nwsite)
      real    vert(nwsite,kzl,5), uutme(nwsite), uutmn(nwsite),
     +        telv(nwsite), xltme(nwsite), xlnmn(nwsite)
*      character csta(nwsite)*6
*
*      data csta/11*'sta001'/
*
c     optional fields                    
      REAL senhtflx(ixl,jyl), lathtflx(ixl,jyl), 
     &     lonwvrad(ixl,jyl),
     &     shtwvrad(ixl,jyl), pblht(ixl,jyl), ustar(ixl,jyl),
     &     pblregime(ixl,jyl), moisture(ixl,jyl),
     &     st1(ixl,jyl), st2(ixl,jyl), st3(ixl,jyl), st4(ixl,jyl),
     &     st5(ixl,jyl), st6(ixl,jyl)

      real eps
      CHARACTER     mdate_ch*8, domain_ch
      CHARACTER*18  precipfile
      character dsetff*100
      character*2 ccyr, ccmo, ccda, cchr
*
      integer jwrite(nzn)
      real zmnlo(nzn), zmnla(nzn), zmxlo(nzn), zmxla(nzn)
*
      common/arrs/uu,vv,junk3,tt,rh,pp,ght,qc,qr,sfp,slp,pstx,pstd,
     + ter,tg,mrf,sih,pmb
      common/arr2/senhtflx,lathtflx,lonwvrad,shtwvrad,pblht,ustar,
     & pblregime,moisture,st1,st2,st3,st4,st5,st6,tke,qv,nci,graupel,
     & sice,cice
* LKB model common statements
      common/pin/uspd,t,q,tsfc,zu,zt,zq,id,p
      common/pout/usr,tsr,qsr,zo,zl,rr,rt,rq
      common/stor1/x3dm, xcm
*
* time/date variables
*
      common/d0/i0yr,i0mo,i0da,i0hr
*
      data pmb/100.,150.,200.,250.,300.,350.,400.,450.,500.,
     +  550.,600.,650.,700.,750.,775.,800.,825.,850.,875.,900.,
     +  925.,950.,960.,970.,980.,990.,1000.,1010.,22*-999./
*
      oahr = 0
      eps = 0.622
      itime=0
*
*&      open(unit=27,file='make_textfls')
      open(unit=27,file='roberts_make_textfls')
      izn=0
   11 read(27,14,end=998)iwrite,xmnlo,xmnla,xmxlo,xmxla
      izn=izn+1
      if(xmnlo.eq.-9999.0)iwrite=0
      if(xmnla.eq.-9999.0)iwrite=0
      if(xmxlo.eq.-9999.0)iwrite=0
      if(xmxla.eq.-9999.0)iwrite=0
      jwrite(izn) = iwrite
      zmnlo(izn) = xmnlo
      zmnla(izn) = xmnla
      zmxlo(izn) = xmxlo
      zmxla(izn) = xmxla
   14 format(5x,i5,4f10.2)
      goto 11
  998 close(27)
      write(*,*)' '
      write(*,*)' ',izn,' = number of WOCSS zones requested'
      write(*,*)' '
*
      open(unit=27,file='mm5.domain')
        read(27,1002)idom
 1002   format(5x,i5)
        read(27,1404)xlatll,xlonll,xlatur,xlonur,xproj
 1404   format(5x,5f8.2)
      close(27)
*
      open(unit=27,file='mm5data.directory')
      read(27,1006)dsetff
 1006 format(a100)
      close(27)
*
      nbds = nblank(dsetff)
*

      if(idom.eq.5)then
* SUPERSUPERSUPERFINE domain
        open(unit=20,name=dsetff(1:nbds)//'fort.45',
     +  status='old',form='unformatted')
      elseif(idom.eq.4)then
* SUPERSUPERFINE domain
        open(unit=20,name=dsetff(1:nbds)//'fort.44',
     +  status='old',form='unformatted')
      elseif(idom.eq.3)then
* SUPERFINE domain
*        open(unit=20,name=dsetff(1:nbds)//'fort.43',
        open(unit=20,name=dsetff(1:nbds)//'xfort.43',
     +  status='old',form='unformatted')
      elseif(idom.eq.2)then
* FINE domain
        open(unit=20,name=dsetff(1:nbds)//'fort.42',
     +  status='old',form='unformatted')
      elseif(idom.eq.1)then
* COARSE domain
        open(unit=20,name=dsetff(1:nbds)//'fort.41',
     +  status='old',form='unformatted')
      endif
CCC      open(unit=21,name='outfile.1')
      open(unit=22,file='tempfile',form='unformatted')
CCC      open(unit=22,name='outfile.2')
*
      iloop=0
 30   read (20,END=101) mif, mrf, mifc, mrfc
      iloop=iloop+1
      iahr = 0
      ibhr = 0
      ftime = mif(1,6)
      mdate = mif(2,6)
      domid = mif(101,1)
      ix = mif(104,1)
      jy = mif(105,1)
      fhour = NINT(mrf(1,6)/60.0)
      kz = nint(mrf(101,6))
      kzs = 1
      kze = kz
*SIGMA ONLY      kzg=26
      kzg=kz
      print *, 'I = ',ix,' J = ',jy,' K = ', kz
      print *, 'Date = ',mif(1,6), ' Forecast Time = ',
     +nint(mrf(1,6)/60.0)
*
      if(ix.gt.140.or.jy.gt.140.or.kz.gt.40) then
         print *, 'Sorry, Model Arrays too big!'
         print *, 'Need to recompile program'
         stop 
      endif
*
CCCCCCCCCCCC
c open separate precip file for the domain, writes header time at hr 0
c         write (mdate_ch, '(i8)') mdate
c         write (domain_ch, '(i1)') domid
c         precipfile=mdate_ch//'_precip_d'//domain_ch
c      if (fhour.eq.0) then
c        open(unit=23,file=precipfile,form='unformatted',
c     +  status='new')
c        print*,'precipfile=',precipfile
c        write(23) mif(1,6)
c        write(23) mif(104,1)
c        write(23) mif(105,1)
c      else
c        open(unit=23,file=precipfile,form='unformatted',
c     +  status='old',access='append')
c      endif  
CCCCCCCCCCCC
*
      iunit=20
      initial=1
      if(initial.eq.1)then
         nlv = nint(mrf(101,6)+1.0)
         vcoord(1) = 0.0
         do kk=2,nlv
          vcoord(kk) = 2.0*mrf(100+kk,5) - vcoord(kk-1)
          a(kk-1) = mrf(100+kk,5)
          write(*,1303)kk,vcoord(kk),a(kk-1)
 1303     format(' level=',i5,' FULL=',e15.7,' HALF=',e15.7)
         enddo
*
         mdate  = mif(2,6)
         idry   = mif(3,6)
         imoist = mif(4,6)
         inhyd  = mif(5,6)
         itgflg = mif(6,6)
         iice   = mif(7,6)
         inav   = mif(8,6)
         iiceg  = mif(9,6)
         maxmv  = mif(10,6)
         jmn    = mif(11,6)
         jhr    = mif(12,6)
         jdy    = mif(13,6)
         jmo    = mif(14,6)
         jyr    = mif(15,6)
         jcry   = mif(16,6)
*
         ifrest = mif(301,6)
         ixtimr = mif(302,6)
         ifsave = mif(303,6)
         iftape = mif(304,6)
         maschk = mif(305,6)
         ifrad  = mif(306,6)
         icustb = mif(307,6)
         iexice = mif(308,6)
         ifdry  = mif(309,6)
         imvdif = mif(310,6)
         ibmst  = mif(311,6)
         icor3d = mif(312,6)
         ifupr  = mif(313,6)
         iboudy = mif(314,6)
         ibltyp = mif(315,6)
         idry   = mif(316,6)
         imoist = mif(317,6)
         icupa  = mif(318,6)
         issflx = mif(319,6)
         itgflg = mif(320,6)
         isfpar = mif(321,6)
         icloud = mif(322,6)
         icdcon = mif(323,6)
         ifsnow = mif(324,6)
         imoiav = mif(325,6)
         ivmixm = mif(326,6)
         ievap  = mif(327,6)
         ishllo = mif(328,6)
         ioverw = mif(329,6)
         imove  = mif(330,6)
         imovco = mif(331,6)
         ifeed  = mif(332,6)
         iabsor = mif(333,6)
*
         P0 = mrf(2,6)
         TS0 = mrf(3,6)
         TLP = mrf(4,6)
*
         savfrq = mrf(301,6)
         tapfrq = mrf(302,6)
         radfrq = mrf(303,6)
         hydpre = mrf(304,6)
         xmoist = mrf(305,6)
*
         timax  = mrf(308,6)
         tistep = mrf(309,6)
         zzlnd  = mrf(310,6)
         zzwtr  = mrf(311,6)
         alblnd = mrf(312,6)
         thinld = mrf(313,6)
         xmava  = mrf(314,6)
         conf   = mrf(315,6)
      endif
*
      n3d    = mif(201,6)
      n2d    = mif(202,6)
      n1d    = mif(203,6)
      n0d    = mif(204,6)
*
      write(*,*)' '
      write(*,*)' n3d=',n3d,' n2d=',n2d,' n1d=',n1d,' n0d=',n0d
      write(*,*)' '
      index = mif(1,1)
* ptop in millibars
      ptop  = mrf(1,2)
      xtime = mrf(1,index)
* convert xtime from minutes to hours
      xtime = xtime/60.0
*
      do n3=1,n3d
        lfld = 204+n3
        field = mifc(lfld,index)(1:8)
        if(initial.eq.1)write(*,1048)n3,field
 1048   format(' #=',i5,' FIELD=',a8,'###')
        read(iunit)((( d3d(i,j,k),i=1,ix),j=1,jy),k=1,kz)
        if(field.eq.'U       ')then
          do ii=1,ix
          do jj=1,jy
          do kk=1,kz
            uu(ii,jj,kk)=d3d(ii,jj,kk)
          enddo
          enddo
          enddo
        elseif(field.eq.'V       ')then
          do ii=1,ix
          do jj=1,jy
          do kk=1,kz
            vv(ii,jj,kk)=d3d(ii,jj,kk)
          enddo
          enddo
          enddo
        elseif(field.eq.'T       ')then
          do ii=1,ix
          do jj=1,jy
          do kk=1,kz
            tt(ii,jj,kk)=d3d(ii,jj,kk)
          enddo
          enddo
          enddo
        elseif(field.eq.'Q       ')then
          do ii=1,ix
          do jj=1,jy
          do kk=1,kz
            qv(ii,jj,kk)=d3d(ii,jj,kk)
          enddo
          enddo
          enddo
        elseif(field.eq.'CLW     ')then
          do ii=1,ix
          do jj=1,jy
          do kk=1,kz
            qc(ii,jj,kk)=d3d(ii,jj,kk)
          enddo
          enddo
          enddo
        elseif(field.eq.'RNW     ')then
          do ii=1,ix
          do jj=1,jy
          do kk=1,kz
            qr(ii,jj,kk)=d3d(ii,jj,kk)
          enddo
          enddo
          enddo
        elseif(field.eq.'ICE     ')then
          do ii=1,ix
          do jj=1,jy
          do kk=1,kz
            cice(ii,jj,kk)=d3d(ii,jj,kk)
          enddo
          enddo
          enddo
        elseif(field.eq.'SNOW    ')then
          do ii=1,ix
          do jj=1,jy
          do kk=1,kz
            sice(ii,jj,kk)=d3d(ii,jj,kk)
          enddo
          enddo
          enddo
        elseif(field.eq.'GRAUPEL ')then
          do ii=1,ix
          do jj=1,jy
          do kk=1,kz
            graupel(ii,jj,kk)=d3d(ii,jj,kk)
          enddo
          enddo
          enddo
        elseif(field.eq.'NCI     ')then
          do ii=1,ix
          do jj=1,jy
          do kk=1,kz
            nci(ii,jj,kk)=d3d(ii,jj,kk)
          enddo
          enddo
          enddo
        elseif(field.eq.'W       ')then
          do ii=1,ix
          do jj=1,jy
          do kk=1,kz
            ww(ii,jj,kk)=d3d(ii,jj,kk)
          enddo
          enddo
          enddo
        elseif(field.eq.'PP      ')then
          do ii=1,ix
          do jj=1,jy
          do kk=1,kz
            pp(ii,jj,kk)=d3d(ii,jj,kk)
          enddo
          enddo
          enddo
        elseif(field.eq.'TKE     ')then
          do ii=1,ix
          do jj=1,jy
          do kk=1,kz
            tke(ii,jj,kk)=d3d(ii,jj,kk)
          enddo
          enddo
          enddo
        elseif(field.eq.'RAD TEND')then
          do ii=1,ix
          do jj=1,jy
          do kk=1,kz
            junk3(ii,jj,kk)=d3d(ii,jj,kk)
          enddo
          enddo
          enddo
        endif
      enddo
*
      do n2=1,n2d
        field = mifc(lfld+n2,index)(1:8)
        if(initial.eq.1)write(*,1048)n2,field
        read(iunit)(( d2d(i,j),i=1,ix),j=1,jy)
        if(field.eq.'PSTARCRS')then
*
* if HYDROSTATIC, ps = surface pressure - PTOP
*
* if NONhydrostatic, ps 
*      =  reference ps
*      =  P0*exp{-T0/TLP+[(T0/TLP)**2-(2*g*ter)/(TLP*R)]**1/2}-PTOP
*      =  prsfc                                               -PTOP
*
*      hence, when ter=0.0 [sea lvl], ps = P0-PTOP, and prsfc = P0
*
          do ii=1,ix
          do jj=1,jy
            pstx(ii,jj)=d2d(ii,jj)
          enddo
          enddo
        elseif(field.eq.'GROUND T')then
          do ii=1,ix
          do jj=1,jy
            tg(ii,jj)=d2d(ii,jj)
          enddo
          enddo
        elseif(field.eq.'RAIN CON')then
          do ii=1,ix
          do jj=1,jy
            rc(ii,jj)=d2d(ii,jj)
          enddo
          enddo
        elseif(field.eq.'RAIN NON')then
          do ii=1,ix
          do jj=1,jy
            re(ii,jj)=d2d(ii,jj)
          enddo
          enddo
        elseif(field.eq.'TERRAIN ')then
          do ii=1,ix
          do jj=1,jy
            ter(ii,jj)=d2d(ii,jj)
          enddo
          enddo
        elseif(field.eq.'MAPFACCR')then
          do ii=1,ix
          do jj=1,jy
            xmap(ii,jj)=d2d(ii,jj)
          enddo
          enddo
        elseif(field.eq.'MAPFACDT')then
          do ii=1,ix
          do jj=1,jy
            dotmap(ii,jj)=d2d(ii,jj)
          enddo
          enddo
        elseif(field.eq.'CORIOLIS')then
          do ii=1,ix
          do jj=1,jy
            dotcor(ii,jj)=d2d(ii,jj)
          enddo
          enddo
        elseif(field.eq.'RES TEMP')then
          do ii=1,ix
          do jj=1,jy
            irst(ii,jj)=d2d(ii,jj)
          enddo
          enddo
        elseif(field.eq.'LATITCRS')then
          do ii=1,ix
          do jj=1,jy
            xlat(ii,jj)=d2d(ii,jj)
          enddo
          enddo
        elseif(field.eq.'LONGICRS')then
          do ii=1,ix
          do jj=1,jy
            xlon(ii,jj)=d2d(ii,jj)
          enddo
          enddo
        elseif(field.eq.'LAND USE')then
          do ii=1,ix
          do jj=1,jy
            xluse(ii,jj)=d2d(ii,jj)
          enddo
          enddo
        elseif(field.eq.'SNOWCOVR')then
          do ii=1,ix
          do jj=1,jy
            xsnowc(ii,jj)=d2d(ii,jj)
          enddo
          enddo
        elseif(field.eq.'PBL HGT ')then
          do ii=1,ix
          do jj=1,jy
            pblht(ii,jj)=d2d(ii,jj)
          enddo
          enddo
        elseif(field.eq.'REGIME  ')then
          do ii=1,ix
          do jj=1,jy
            pblregime(ii,jj)=d2d(ii,jj)
          enddo
          enddo
        elseif(field.eq.'SHFLUX  ')then
          do ii=1,ix
          do jj=1,jy
            senhtflx(ii,jj)=d2d(ii,jj)
          enddo
          enddo
        elseif(field.eq.'LHFLUX  ')then
          do ii=1,ix
          do jj=1,jy
            lathtflx(ii,jj)=d2d(ii,jj)
          enddo
          enddo
        elseif(field.eq.'UST     ')then
          do ii=1,ix
          do jj=1,jy
            ustar(ii,jj)=d2d(ii,jj)
          enddo
          enddo
        elseif(field.eq.'SWDOWN  ')then
          do ii=1,ix
          do jj=1,jy
            shtwvrad(ii,jj)=d2d(ii,jj)
          enddo
          enddo
        elseif(field.eq.'LWDOWN  ')then
          do ii=1,ix
          do jj=1,jy
            lonwvrad(ii,jj)=d2d(ii,jj)
          enddo
          enddo
        elseif(field.eq.'SOIL T 1')then
          do ii=1,ix
          do jj=1,jy
            st1(ii,jj)=d2d(ii,jj)
          enddo
          enddo
        elseif(field.eq.'SOIL T 2')then
          do ii=1,ix
          do jj=1,jy
            st2(ii,jj)=d2d(ii,jj)
          enddo
          enddo
        elseif(field.eq.'SOIL T 3')then
          do ii=1,ix
          do jj=1,jy
            st3(ii,jj)=d2d(ii,jj)
          enddo
          enddo
        elseif(field.eq.'SOIL T 4')then
          do ii=1,ix
          do jj=1,jy
            st4(ii,jj)=d2d(ii,jj)
          enddo
          enddo
        elseif(field.eq.'SOIL T 5')then
          do ii=1,ix
          do jj=1,jy
            st5(ii,jj)=d2d(ii,jj)
          enddo
          enddo
        elseif(field.eq.'SOIL T 6')then
          do ii=1,ix
          do jj=1,jy
            st6(ii,jj)=d2d(ii,jj)
          enddo
          enddo
        endif
      enddo
*
      do n1=1,n1d
        read(iunit)( d1d(i),i=1,ix)
        if(n1.eq.1)then
        elseif(n1.eq.2)then
        endif
      enddo
*
      do n0=1,n0d
        read(iunit)d0d
      enddo
*
      if(initial.eq.1)then
        if(inhyd.eq.0)then
          write(*,*)' HYDROSTATIC MM5 output'
          do ii=1,ix
          do jj=1,jy
          do kk=1,kz
            pp(ii,jj,kk)=0.0
            ww(ii,jj,kk)=0.0
          enddo
          enddo
          enddo
*
          do ii=1,ix
          do jj=1,jy
            ww(ii,jj,kz+1)=0.0
          enddo
          enddo
        endif
        if(inhyd.eq.1)write(*,*)' NONhydrostatic MM5 output'
        initial = 0
      endif
*
      write(*,*)' Date=',mif(1,6),' xtime=',xtime
      write(*,*)' '
*^^^^^^^^^^^^^^^^^^^^ NEW ^^^^^^^^^^^^^^^^^^^^^^^^^^^
*vvvvvvvvvvvvvvvvvvvv OLD vvvvvvvvvvvvvvvvvvvvvvvvvvv
*C         1) U COMPONENT WIND
*      print*,'reading uu winds'
*      read (20) ((( uu(i,j,k),i=1,ix),j=1,jy),k=1,kz)
*C         2) V COMPONENT WIND
*      print*,'reading vv winds'
*      read (20) ((( vv(i,j,k),i=1,ix),j=1,jy),k=1,kz)
*C         3) TEMPERATURE
*      print*,'reading temperature'
*      read (20) ((( tt(i,j,k),i=1,ix),j=1,jy),k=1,kz)
*C         4) QV
*      print*,'reading qv'
*      read (20) ((( qv(i,j,k),i=1,ix),j=1,jy),k=1,kz)
*C         5) QV CLOUD
*      print*,'reading qv cloud'
*      read (20) ((( qc(i,j,k),i=1,ix),j=1,jy),k=1,kz)
*C         6) QV RAIN
*      print*,'reading qv rain'
*      read (20) ((( qr(i,j,k),i=1,ix),j=1,jy),k=1,kz)
*      if(mif(7,6).eq.1) then
*C          6.1) Cloud ice
*         print*,'reading cloud ice'
*         read (20) ((( cice(i,j,k),i=1,ix),j=1,jy),k=1,kz)
*C          6.2) snow ice
*         print*,'reading snow ice'
*         read (20) ((( sice(i,j,k),i=1,ix),j=1,jy),k=1,kz)
*      endif
*
*C        6.5) TURBULENT KINETIC ENERGY
*      if(mif(315,6).eq.3)then
*         print*,'reading TKE'
**         read (20) ((( tke(i,j,k),i=1,ix),j=1,jy),k=1,kz)
*         read (20) ((( xtke,i=1,ix),j=1,jy),k=1,kz)
*      endif
*
*C        7) RAD TENDENCY
*      print*,'reading rad tend'
*      read (20) ((( junk3(i,j,k),i=1,ix),j=1,jy),k=1,kz)
*C        8) VERTICAL WIND COMPONENT
*      print*,'reading ww'
*      read (20) ((( ww(i,j,k),i=1,ix),j=1,jy),k=1,kz+1)
*C        9) PRESSURE PERTURBATION
*      print*,'reading press pert'
*      read (20) ((( pp(i,j,k),i=1,ix),j=1,jy),k=1,kz)
*CCCCCCCCCCCC
*C         1) PSTAR
*      print*,'reading pstar'
*      read (20) ((pstx(i,j),i=1,ix),j=1,jy)
*      print*,'pstx(5,5) = ',pstx(5,5)
*C         2) GROUND TEMPERATURE
*      print*,'reading ground temp'
*      read (20) ((tg(i,j),i=1,ix),j=1,jy)
*C         3) ACCUMULATED CONVECTIVE PRECIPITATION
*      read (20) ((rc(i,j),i=1,ix),j=1,jy)
*C         4) ACCUMULATED RESOLVED PRECIPITATION
*      read (20) ((re(i,j),i=1,ix),j=1,jy)
*C         5) TERRAIN
*      read (20) ((ter(i,j),i=1,ix),j=1,jy)
*C         6) MAP FACTOR CROSS
*      read (20) ((xmap(i,j),i=1,ix),j=1,jy)
*C         7) MAP FACTOR DOT
*      read (20) ((dotmap(i,j),i=1,ix),j=1,jy)
*C         8) CORIOLIS
*      read (20) ((dotcor(i,j),i=1,ix),j=1,jy)
*C         9) (OPTIONAL) RESERVOIR TEMPERATURE
*      read (20) ((irst(i,j),i=1,ix),j=1,jy)
*C         10) LATITUDE
*      read (20) ((xlat(i,j),i=1,ix),j=1,jy)
*C         11) LONGITUDE
*      read (20) ((xlon(i,j),i=1,ix),j=1,jy)
*C         12) LAND USE CATEGORY
*      read (20) ((xluse(i,j),i=1,ix),j=1,jy)
*C         13) SNOW COVER
*      read (20) ((xsnowc(i,j),i=1,ix),j=1,jy)
*C         14) PLANETARY BOUNDARY LAYER HEIGHT
*      READ (20) ((pblht(i,j),i=1,ix),j=1,jy)
*C         15) PBL REGIME
*      READ (20) ((pblregime(i,j),i=1,ix),j=1,jy)
*C         16) SENSIBLE HEAT FLUX
*      READ (20) ((senhtflx(i,j),i=1,ix),j=1,jy)
*C         17) LATENT HEAT FLUX
*      READ (20) ((lathtflx(i,j),i=1,ix),j=1,jy)
*C         18) FRICTION VELOCITY
*      READ (20) ((ustar(i,j),i=1,ix),j=1,jy)
*C         19) SHORTWAVE RADIATION
*      READ (20) ((shtwvrad(i,j),i=1,ix),j=1,jy)
*C         20) LONGWAVE RADIATION
*      READ (20) ((lonwvrad(i,j),i=1,ix),j=1,jy)
*C         21) SOIL T1
*      READ (20) ((st1(i,j),i=1,ix),j=1,jy)
*C         22) SOIL T2
*      READ (20) ((st2(i,j),i=1,ix),j=1,jy)
*C         23) SOIL T3
*      READ (20) ((st3(i,j),i=1,ix),j=1,jy)
*C         24) SOIL T4
*      READ (20) ((st4(i,j),i=1,ix),j=1,jy)
*C         25) SOIL T5
*      READ (20) ((st5(i,j),i=1,ix),j=1,jy)
*C         26) SOIL T6
*      READ (20) ((st6(i,j),i=1,ix),j=1,jy)
*
cc      PRINT*,'done reading model output, now filling arrays'
C
CCCCCCCCCCCCCCCCCCCCCCCC
*^^^^^^^^^^^^^^^^^^^^ OLD ^^^^^^^^^^^^^^^^^^^^^^^^^^^

      print*,'ww(5,5,5) = ',ww(5,5,5)
      print*,'ww(10,10,10) = ',ww(10,10,10)
      print*,'ww(10,10,10) = ',ww(10,10,10)
      print*,'pstx(5,5) = ', pstx(i,j)
      do 648 aa = 1,kz
         sih(aa) = mrf(101+aa,6)
 648  continue

      do 945 aa = 1,ix
         do 945 bb = 1,jy
               if(aa.eq.ix.or.bb.eq.jy) then
                  if(aa.eq.ix) then
                     xluse(aa,bb) = xluse(aa-1,bb)
                  endif
                  if(bb.eq.jy) then
                     xluse(aa,bb) = xluse(aa,bb-1)
                  endif
               endif
 945  continue
CCCCC
CC calculate xcor from dotcor points
CCCCC
      do 189 i=1,ix-1
        do 189 j=1,jy-1
           xcor(i,j)= 0.25*(dotcor(i,j)+dotcor(i+1,j)
     &       +dotcor(i,j+1)+dotcor(i+1,j+1))
           tot_prec(i,j) = (10.0*rc(i,j))+(10.0*re(i,j))
           rc(i,j) = 10.0*rc(i,j)
           re(i,j) = 10.0*re(i,j)
 189  continue

      do 190 i=1,ix
        do 190 j=1,jy
           aa = min(i,ix-1)
           bb = min(j,jy-1)
           cc = max(i-1,1)
           dd = max(j-1,1)
           pstd(i,j)= 0.25*(pstx(aa,bb)+pstx(cc,bb)
     &       +pstx(aa,dd)+pstx(cc,dd))
 190  continue
CC
C Decouple Data
CC
      do 458 i=1,ix
         do 458 j=1,jy
            do 458 k=1,kz
               uu(i,j,k) = uu(i,j,k)/pstd(i,j)
               vv(i,j,k) = vv(i,j,k)/pstd(i,j)
 458  continue

      do 191 i=1,ix-1
         do 191 j=1,jy-1
            do 191 k=1,kz
               tt(i,j,k) = tt(i,j,k)/pstx(i,j)
               qv(i,j,k) = qv(i,j,k)/pstx(i,j)
               qc(i,j,k) = qc(i,j,k)/pstx(i,j)
               qr(i,j,k) = qr(i,j,k)/pstx(i,j)
               pp(i,j,k) = 0.01*pp(i,j,k)/pstx(i,j)
 191  continue
C
      do 404 i=1,ix-1
         do 404 j=1,jy-1
            do 404 k=1,kz+1
               ww(i,j,k) = ww(i,j,k)/pstx(i,j)
 404  continue
*
* Interpolate W from FULL sigma levels to HALF sigma levels
*
      do 406 i=1,ix-1
         do 406 j=1,jy-1
            do 406 k=1,kz
               junk3(i,j,k) = (ww(i,j,k)+ww(i,j,k+1))/2.0
 406  continue
  
               print*,'ww(5,5,5) = ',ww(5,5,5)
C
      do 192 i=1,ix-1
         do 192 j=1,jy-1
            pstx(i,j)=pstx(i,j)*10.0
 192  continue
C
      do 193 i=1,ix
         do 193 j=1,jy
            pstd(i,j)=pstd(i,j)*10.0
 193     continue
CCCCC
C Calculate Surface Pressure
CCCCC
      do 847 i=1,ix-1
         do 847 j=1,jy-1
            sfp(i,j)=pstx(i,j)+mrf(1,2)+pp(i,j,kz)
C
            wmixrat=qv(i,j,kz)
            tvll=tt(i,j,kz) * (1.+0.608*wmixrat )
            beta=287.04/9.81*
     &           log((pstx(i,j)+mrf(1,2)+pp(i,j,kz))/
     &           (sih(kz)*pstx(i,j)+mrf(1,2)+pp(i,j,kz)))
            gpll=ter(i,j)+beta*tvll/(1.-.5*beta*0.0065)
            tvavg = tvll + 0.0065 * (gpll - .5 * ter(i,j))
            slp(i,j)=(pstx(i,j)+mrf(1,2)+pp(i,j,kz))*
     &           exp(9.81*ter(i,j)/287.04/tvavg)
 847  continue

C
      DO j=1,jy-1
         DO i=1,ix-1
            sfp(i,j)=pstx(i,j)+mrf(1,2)+pp(i,j,kz)
*
* compute integrated water vapor
*
            ilow=0
            imid=0
            ihi =0
            DO k=1,kz
               p=pstx(i,j)*sih(k)+mrf(1,2)+pp(i,j,k)
               if((p.le.1100.0).and.(p.ge.700.0))then
                 ilow=ilow+1
*                 flow(ilow)=qv(i,j,k)
                 flow(ilow)=qv(i,j,k)/(qv(i,j,k)+1.0)
                 plow(ilow)=p*100.
               elseif((p.lt.700.0).and.(p.ge.400.0))then
                 imid=imid+1
*                 fmid(imid)=qv(i,j,k)
                 fmid(imid)=qv(i,j,k)/(qv(i,j,k)+1.0)
                 pmid(imid)=p*100.
               elseif((p.lt.400.0).and.(p.ge.0.0))then
                 ihi =ihi +1
*                 fhi(ihi)=qv(i,j,k)
                 fhi(ihi)=qv(i,j,k)/(qv(i,j,k)+1.0)
                 phi(ihi)=p*100.
               endif
            END DO
            call cctu(flow,plow,ilow,cldl)
            call cctu(fmid,pmid,imid,cldm)
            call cctu(fhi,phi,ihi,cldh)
            qvtot(i,j)=(cldl+cldm+cldh)/9.81
*
* compute integrated rain water
*
            ilow=0
            imid=0
            ihi =0
            DO k=1,kz
               p=pstx(i,j)*sih(k)+mrf(1,2)+pp(i,j,k)
               if((p.le.1100.0).and.(p.ge.700.0))then
                 ilow=ilow+1
*                 flow(ilow)=qr(i,j,k)
                 flow(ilow)=qr(i,j,k)/(qr(i,j,k)+1.0)
                 plow(ilow)=p*100.
               elseif((p.lt.700.0).and.(p.ge.400.0))then
                 imid=imid+1
*                 fmid(imid)=qr(i,j,k)
                 fmid(imid)=qr(i,j,k)/(qr(i,j,k)+1.0)
                 pmid(imid)=p*100.
               elseif((p.lt.400.0).and.(p.ge.0.0))then
                 ihi =ihi +1
*                 fhi(ihi)=qr(i,j,k)
                 fhi(ihi)=qr(i,j,k)/(qr(i,j,k)+1.0)
                 phi(ihi)=p*100.
               endif
            END DO
            call cctu(flow,plow,ilow,cldl)
            call cctu(fmid,pmid,imid,cldm)
            call cctu(fhi,phi,ihi,cldh)
            qrtot(i,j)=(cldl+cldm+cldh)/9.81
*
* compute integrated cloud water
C calculate relative humidity
*
            ilow=0
            imid=0
            ihi =0
            DO k=1,kz
               q=MAX(qv(i,j,k),1.e-15)
               t=tt(i,j,k)
               tcels=t-273.15
               p=pstx(i,j)*sih(k)+mrf(1,2)+pp(i,j,k)
               esat=6.112*exp((17.67*tcels)/(tcels+243.5))
               qsat=eps*esat/p 
               rh(i,j,k)=q/qsat*100
               if((p.le.1100.0).and.(p.ge.700.0))then
                 ilow=ilow+1
*                 flow(ilow)=qc(i,j,k)
                 flow(ilow)=qc(i,j,k)/(qc(i,j,k)+1.0)
                 plow(ilow)=p*100.
               elseif((p.lt.700.0).and.(p.ge.400.0))then
                 imid=imid+1
*                 fmid(imid)=qc(i,j,k)
                 fmid(imid)=qc(i,j,k)/(qc(i,j,k)+1.0)
                 pmid(imid)=p*100.
               elseif((p.lt.400.0).and.(p.ge.0.0))then
                 ihi =ihi +1
*                 fhi(ihi)=qc(i,j,k)
                 fhi(ihi)=qc(i,j,k)/(qc(i,j,k)+1.0)
                 phi(ihi)=p*100.
               endif
            END DO
            call cctu(flow,plow,ilow,cldl)
            call cctu(fmid,pmid,imid,cldm)
            call cctu(fhi,phi,ihi,cldh)
            qctot(i,j)=(cldl+cldm+cldh)/9.81
            xlow(i,j)=cldl/9.81
            xmid(i,j)=cldm/9.81
            xhi(i,j) =cldh/9.81
*********************
*            write(*,7891)itime,i,j,ilow,imid,ihi
* 7891       format(6i5)
*            if((abs(cldl).gt.1.0).or.(abs(cldm).gt.1.0).or.
*     +       (abs(cldh).gt.1.0))then
*            write(*,7893)itime,i,j,cldl,cldm,cldh
* 7893       format(3i5,3e15.7)
*            endif
*          if((itime.eq.10).and.(i.eq.99).and.(j.eq.26))then
* 1534       format(4e15.7)
*            write(*,*)' LOW'
*            write(*,1534)(flow(i3),i3=1,ilow)
*            write(*,1534)(plow(i3),i3=1,ilow)
*            write(*,*)' MID'
*            write(*,1534)(fmid(i3),i3=1,imid)
*            write(*,1534)(pmid(i3),i3=1,imid)
*            write(*,*)' HI'
*            write(*,1534)(fhi(i3),i3=1,ihi)
*            write(*,1534)(phi(i3),i3=1,ihi)
*            write(*,7895)itime,i,j,cldl,cldm,cldh
* 7895       format(3i5,3e15.7)
*          endif
*********************
         END DO 
      END DO
      print*,'t',t,' p ',p,' q ',q,' qsat ',qsat
      print*,'eps ',eps,' esat ',esat
      PRINT*,'rh(5,5,5) = ',rh(5,5,5) 
*********************
*      if(itime.eq.10)STOP
*      STOP
*********************

C Calculate dewpoint
      do 727 k=1,kz
         do 727 j=1,jy-1
            do 727 i=1,ix-1
               q=max(qv(i,j,k),1.e-15)
               t=tt(i,j,k)
               p=pstx(i,j)*sih(k)+mrf(1,2)+pp(i,j,k)
               e=q*p/(0.622+(1.-0.622)*q)
               dew(i,j,k)=5418.12/(19.84659-alog(e/6.112))
 727  continue

CCCCCC
C Calculate Rain totals
      output_hr = NINT(mrf(1,6)/60.0)
ccc      print*,'output_hr ',output_hr
      do j=1, jy-1
         do i=1, ix-1
            prec_3hr(i,j)=0.0
            prcn_3hr(i,j)=0.0
         end do
      end do

c
c     unless this is the initialization time, compute the precip
c

      if (output_hr.ne.0) then
      open(unit=22,file='tempfile',form='unformatted')
      open(unit=23,file='tcnvfile',form='unformatted')
         print*,'reading the last output from tempfile'
         read(22) last_output_hr
         if ((output_hr-last_output_hr) .ne. 3) then
ccc            print*,'missing the last output file precip'
         else
            read(22) last_output_hr_tot_prec
            read(23) rcb
            do j=1,jy-1            
               do i=1,ix-1
                  prec_3hr(i,j)=tot_prec(i,j)-
     &                 last_output_hr_tot_prec(i,j)
ccc                  print*,'prec_3hr(i,j)',prec_3hr(i,j)
                  prcn_3hr(i,j)=rc(i,j)-rcb(i,j)
               end do
            end do
         endif
      close(22)
      close(23)
      endif

c   write the current total precip grid to the tempfile      
*      rewind (22)
      open(unit=22,file='tempfile',form='unformatted')
      open(unit=23,file='tcnvfile',form='unformatted')
      write(22) output_hr
      write(22) tot_prec
      write(23) rc
      close(22)
      close(23)

*
*  Convert from Sigma Lvls on Cross points to 
*  Pressure Lvls on Cross points.

*SIGMA ONLY      call sig2pr(ix,jy,kz,kzg,ixl,jyl,kzl)
      call ghtsig(ix,jy,kz)
*
*SIGMA ONLY      kz=kzg+1
      kz=kzg
*
C Calculate dewpoint
      do 728 k=1,kz
*         p=pmb(k)
*SIGMA ONLY         ilvla(k) = nint(p)
         do 728 j=1,jy-1
            do 728 i=1,ix-1
*SIGMA ONLY      if(k.eq.kz)p=pstx(i,j)*sih(k)+mrf(1,2)+pp(i,j,k)
               p=pstx(i,j)*sih(k)+mrf(1,2)+pp(i,j,k)
               pr(i,j,k)=p
               if(rh(i,j,k).lt.-500.0)then
                 dew(i,j,k)=-9999.0
                 tt(i,j,k)=-9999.0
               else
                 rrh=rh(i,j,k)/100.
                 t=tt(i,j,k)
                 if(t.lt.100.)then
                    write(*,*)i,j,k,p,t
                    write(*,*)' unrealistic TEMP, STOP'
                    STOP 1433
                 endif
                 qqv=xmr(p,t,rrh)
                 q=max(qqv,1.e-15)
                 e=q*p/(0.622+(1.-0.622)*q)
                 dew(i,j,k)=5418.12/(19.84659-alog(e/6.112))
               endif
*********************
**        if((i.eq.1).and.(j.eq.1))then
*        if((i.eq.51).and.(j.eq.80))then
*        write(*,8694)i,j,k,
*     + junk3(i,j,k), tt(i,j,k), dew(i,j,k), uu(i,j,k), vv(i,j,k)
* 8694   format(3i5,5e15.7)
*        endif
*********************
 728  continue

CCCCCCCCCCC
C
C Interp to dot points
C
CCCCCCCCCCC
      do 2679 i=1,ix
         do 2679 j=1,jy
            do 2679 k=1,kz
               aa = min(i,ix-1)
               bb = min(j,jy-1)
               cc = max(i-1,1)
               dd = max(j-1,1)
               sumw=0.0
               sumt=0.0
               sumd=0.0
               sumr=0.0
               sumg=0.0
               sumqc=0.0
               sumqr=0.0
               sumpr=0.0
               wpt=0.0
               tpt=0.0
               dpt=0.0
               rpt=0.0
               gpt=0.0
               qcpt=0.0
               qrpt=0.0
               prpt=0.0
*
               do ipp=1,4
                 if(ipp.eq.1)then
                   wpar=junk3(aa,bb,k)
                   tpar=tt(aa,bb,k)
                   dpar=dew(aa,bb,k)
                   qcpar=qc(aa,bb,k)
                   qrpar=qr(aa,bb,k)
                   rpar=rh(aa,bb,k)
                   gpar=ght(aa,bb,k)
                   ppar=pr(aa,bb,k)
                 elseif(ipp.eq.2)then
                   wpar=junk3(cc,bb,k)
                   tpar=tt(cc,bb,k)
                   dpar=dew(cc,bb,k)
                   qcpar=qc(cc,bb,k)
                   qrpar=qr(cc,bb,k)
                   rpar=rh(cc,bb,k)
                   gpar=ght(cc,bb,k)
                   ppar=pr(cc,bb,k)
                 elseif(ipp.eq.3)then
                   wpar=junk3(aa,dd,k)
                   tpar=tt(aa,dd,k)
                   dpar=dew(aa,dd,k)
                   qcpar=qc(aa,dd,k)
                   qrpar=qr(aa,dd,k)
                   rpar=rh(aa,dd,k)
                   gpar=ght(aa,dd,k)
                   ppar=pr(aa,dd,k)
                 elseif(ipp.eq.4)then
                   wpar=junk3(cc,dd,k)
                   tpar=tt(cc,dd,k)
                   dpar=dew(cc,dd,k)
                   qcpar=qc(cc,dd,k)
                   qrpar=qr(cc,dd,k)
                   rpar=rh(cc,dd,k)
                   gpar=ght(cc,dd,k)
                   ppar=pr(cc,dd,k)
                 endif
                 if(wpar.ne.-9999.0)then
                   sumw=sumw+wpar
                   wpt=wpt+1.0
                 endif
                 if(tpar.ne.-9999.0)then
                   sumt=sumt+tpar
                   tpt=tpt+1.0
                 endif
                 if(dpar.ne.-9999.0)then
                   sumd=sumd+dpar
                   dpt=dpt+1.0
                 endif
                 if(rpar.ne.-9999.0)then
                   sumr=sumr+rpar
                   rpt=rpt+1.0
                 endif
                 if(gpar.ne.-9999.0)then
                   sumg=sumg+gpar
                   gpt=gpt+1.0
                 endif
                 if(qcpar.ne.-9999.0)then
                   sumqc=sumqc+qcpar
                   qcpt=qcpt+1.0
                 endif
                 if(qrpar.ne.-9999.0)then
                   sumqr=sumqr+qrpar
                   qrpt=qrpt+1.0
                 endif
                 if(ppar.ne.-9999.0)then
                   sumpr=sumpr+ppar
                   prpt=prpt+1.0
                 endif
               enddo
*
               if(wpt.ge.1.0)then
                 wwd(i,j,k)=sumw/wpt
               else
                 wwd(i,j,k)=-9999.0
               endif
               if(tpt.ge.1.0)then
                 ttd(i,j,k)=sumt/tpt
               else
                 ttd(i,j,k)=-9999.0
               endif
               if(dpt.ge.1.0)then
                 dewd(i,j,k)=sumd/dpt
               else
                 dewd(i,j,k)=-9999.0
               endif
               if(rpt.ge.1.0)then
                 rhd(i,j,k)=sumr/rpt
               else
                 rhd(i,j,k)=-9999.0
               endif
               if(gpt.ge.1.0)then
                 ghtd(i,j,k)=sumg/gpt
               else
                 ghtd(i,j,k)=-9999.0
               endif
               if(qcpt.ge.1.0)then
                 clw(i,j,k)=sumqc/qcpt
               else
                 clw(i,j,k)=-9999.0
               endif
               if(qrpt.ge.1.0)then
                 rnw(i,j,k)=sumqr/qrpt
               else
                 rnw(i,j,k)=-9999.0
               endif
               if(prpt.ge.1.0)then
                 prd(i,j,k)=sumpr/prpt
               else
                 prd(i,j,k)=-9999.0
               endif
 2679 continue

      do 2680 i=1,ix
         do 2680 j=1,jy
               aa = min(i,ix-1)
               bb = min(j,jy-1)
               cc = max(i-1,1)
               dd = max(j-1,1)
               sfpd(i,j) = 0.25*(sfp(aa,bb)+sfp(cc,bb)
     &              +sfp(aa,dd)+sfp(cc,dd))
               slpd(i,j) = 0.25*(slp(aa,bb)+slp(cc,bb)
     &              +slp(aa,dd)+slp(cc,dd))
               tgd(i,j) = 0.25*(tg(aa,bb)+tg(cc,bb)
     &              +tg(aa,dd)+tg(cc,dd))
               terd(i,j) = 0.25*(ter(aa,bb)+ter(cc,bb)
     &              +ter(aa,dd)+ter(cc,dd))
               riad(i,j) = 0.25*(prec_3hr(aa,bb)+prec_3hr(cc,bb)
     &              +prec_3hr(aa,dd)+prec_3hr(cc,dd))
               ribd(i,j) = 0.25*(prcn_3hr(aa,bb)+prcn_3hr(cc,bb)
     &              +prcn_3hr(aa,dd)+prcn_3hr(cc,dd))
               vqcl(i,j) = 0.25*(xlow(aa,bb)+xlow(cc,bb)
     &              +xlow(aa,dd)+xlow(cc,dd))
               vqcm(i,j) = 0.25*(xmid(aa,bb)+xmid(cc,bb)
     &              +xmid(aa,dd)+xmid(cc,dd))
               vqch(i,j) = 0.25*(xhi(aa,bb)+xhi(cc,bb)
     &              +xhi(aa,dd)+xhi(cc,dd))
 2680 continue

*
* Compute Lapse Rate (LAPS) [K m**-1]
*
      do 2681 i=1,ix
        do 2681 j=1,jy
          do 2681 k=2,kz
          igo=1
          if(ttd(i,j,k).eq.-9999.0)igo=0
          if(ghtd(i,j,k).eq.-9999.0)igo=0
          if(ttd(i,j,k-1).eq.-9999.0)igo=0
          if(ghtd(i,j,k01).eq.-9999.0)igo=0
          if(igo.eq.1)then
            dt=ttd(i,j,k-1)-ttd(i,j,k)
            dz=ghtd(i,j,k-1)-ghtd(i,j,k)
            dtdz(i,j,k)=dt/dz
          else
            dtdz(i,j,k)=-9999.0
          endif
 2681 continue

      do 2682 i=1,ix
        do 2682 j=1,jy
            dtdz(i,j,1)=dtdz(i,j,2)
 2682 continue



CCCCCCCCCCCCCCCCCCCCCCCCCCC
C Calculate Map area
CCCCCCCCCCCCCCCCCCCCCCCCCCC
      refrat=mrf(1,1)/mrf(101,1)
      yllc=mif(106,1)+(0)/refrat
      yurc=mif(106,1)+(ix-1.)/refrat-2.0
      xllc=mif(107,1)+(0)/refrat
      xurc=mif(107,1)+(jy-1.)/refrat-2.0
**********************
      write(UNIT=tgarea,FMT='(F8.3,A1,F8.3,A1,F8.3,A1,F8.3)')
     +xlatll,';',xlonll,';',xlatur,';',xlonur
      write(UNIT=tproj, FMT='(A9,F8.3,A5)')
     +'LCC/30.0;',mrf(3,1),'/60.0'
      write(UNIT=tkxky, FMT='(I3,A1,I3)')
     +jy,';',ix

      time = nint(mrf(1,6)/60.0) 

ccc      if (time.le.12) then
ccc          filnum=21
ccc      elseif(time.gt.12) then
ccc          filnum=22
ccc      endif
      filnum=21
      print *, 'GRDAREA=',tgarea
      print *, 'PROJ=',tproj
      print *, 'KXKY=',tkxky


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCC
C Dump Out Arrays for Gempak
CCCCC
*
* Compute TIME1 parameter
      itime=itime+1
      ftim = nint(mrf(1,6)/60.0)
      iyr = mif(2,6)/100
      ihr = mod(mif(2,6),100)*100
      ilvl = nint(mrf(101+ii,6)*10000.0)
      open(unit=37,file='DATTIM')
* Y2K bug!!
*      write(37,1579) iyr,'/',ihr,'F',ftim
* 1579 format(I6,A1,I4.4,A1,I3.3)
      iyr1(1)=mif(2,6)/(10**(7))
      do idig=2,6
        isum=0
        do jdig=2,idig
          isum=isum+iyr1(jdig-1)*(10**(8-jdig+1))
        enddo
        iyr=(mif(2,6)-isum)/(10**(8-idig))
        iyr1(idig)=iyr
*        write(*,*)mif(2,6),isum
*        write(*,*)' idig,iyr,(8-idig+1)=',idig,iyr,(8-idig+1)
      enddo
      do ic=1,6
        if(iyr1(ic).eq.0)then
          cyr(ic:ic)='0'
        elseif(iyr1(ic).eq.1)then
          cyr(ic:ic)='1'
        elseif(iyr1(ic).eq.2)then
          cyr(ic:ic)='2'
        elseif(iyr1(ic).eq.3)then
          cyr(ic:ic)='3'
        elseif(iyr1(ic).eq.4)then
          cyr(ic:ic)='4'
        elseif(iyr1(ic).eq.5)then
          cyr(ic:ic)='5'
        elseif(iyr1(ic).eq.6)then
          cyr(ic:ic)='6'
        elseif(iyr1(ic).eq.7)then
          cyr(ic:ic)='7'
        elseif(iyr1(ic).eq.8)then
          cyr(ic:ic)='8'
        elseif(iyr1(ic).eq.9)then
          cyr(ic:ic)='9'
        endif
      enddo
      write(37,1579) cyr,'/',ihr,'F',ftim
 1579 format(A6,A1,I4.4,A1,I3.3)
      close(37)
      open(unit=37,file='DATTIM')
      read(37,1577)fdatim
 1577 format(a15)
      close(37)
      if(itime.eq.1)then
          if(iwrite.eq.1)then
            open(unit=67,file='mry1sta.'//cyr(1:2))
          elseif(iwrite.eq.2)then
            open(unit=67,file='sfo1sta.'//cyr(1:2))
          elseif(iwrite.eq.3)then
            open(unit=67,file='rob1sta.'//cyr(1:2))
          endif
          open(unit=37,file='curryr')
          read(37,1097)curryr
 1097     format(1x,a4)
          close(37)
          read(fdatim(1:2),'(i2)')i0yr
          read(fdatim(3:4),'(i2)')i0mo
          read(fdatim(5:6),'(i2)')i0da
          read(fdatim(8:9),'(i2)')i0hr
      endif
*
      iwc=66
      do iwrite=1,izn
        iwc=iwc+1
        if(izn.gt.1)then
          write(*,*)' '
          write(*,*)'  NEED to redesign v2_get_wocss_obs.f '
          write(*,*)'  STOP '
          write(*,*)' '
          stop 21073
        endif
*OLDW        write(iwc,6711)ihr/100,cyr(3:4),cyr(5:6),curryr,ftim
*OLDW 6711   format(5x,i2.2,2a2,a4,1x,I3.3)
*OLDW        do ii=1,nwsite
*OLDW          idop(ii)=ii
*OLDW        enddo
*OLDW        write(iwc,*)' '
*OLDW        write(iwc,6721)(idop(ii),ii=1,nwsite)
*OLDW        write(iwc,*)' '
*OLDW        write(iwc,6721)(idop(ii),ii=1,nwsite)
*OLDW 6721   format(1000(5(i3,9x),/))
*OLDW        write(iwc,6731)nwsite
*OLDW 6731   format(5x,i8)
        call cnvric(0,0,0,ftim,ccyr,ccmo,ccda,cchr)
        read(ccmo,'(i2)')imo0
        read(ccda,'(i2)')ida0
        read(cchr,'(i2)')ihr0
        curyr=curryr
        if(curryr(3:4).ne.ccyr)then
           read(curryr,'(i4)')icrryr
           icryr=icrryr+1
           open(unit=37,file='curyr')
           write(37,1098)icryr
 1098      format(1x,i4)
           close(37)
           open(unit=37,file='curyr')
           read(37,1097)curyr
           close(37)
        endif
        write(iwc,6711)ihr0,'00',imo0,ida0,curyr
 6711   format(i2,a2,2i2,1x,a4)
        write(iwc,6731)nwsite,nwsite,nwsite
 6731   format(3i10)
      enddo
*
      ipar=0
 1234 ipar=ipar+1
* IPAR=1
      print *, 'Dumping U - ', uu(1,1,kz)
      print *, '  IPAR=',ipar
      vcord='SGMA'
*SIGMA ONLY      vcord='PRES'
      parm='UREL'
      vcorda(ipar)=vcord
      parma(ipar)=parm
      do 40 ii = kzs,kze 
         ftim = nint(mrf(1,6)/60.0)
         iyr = mif(2,6)/100
         ihr = mod(mif(2,6),100)*100
*SIGMA ONLY
         ilvla(ii) = nint(mrf(101+ii,6)*10000.0)
*         write(21,1581) iyr,'/',ihr,'F',ftim,ilvl,vcord,parm
 1581    format(I6,A1,I4.4,A1,I3.3,3x,I5,3x,A4,3x,A4) 
*         write(21,1583) 'COLUMNS: 1 ',jy-2,'ROWS: 1 ',ix-2
 1583    format(A11,I3,10x,A10,I3)
*         write(21,1584) ' '
 1584    format(A2)
*         do 41 kk =ix-1,2,-1
*            write(21, 1586) 'ROW ',kk-1
* 1586       format(A4,I3)
*            write(21,1587) ((uu(kk,jj,ii)),jj=2,jy-1)
* 1587       format(4f12.4)
* 41      continue
*         write(21,1584) ' '
 40   continue
*SIGMA ONLY      ilvla(kze+1) = nint(mrf(1,2))
*SIGMA ONLY      ilvla(kz) = nint(mrf(101+kze,6)*10000.0)
 
      ipar=ipar+1
* IPAR=2
      print *, 'Dumping V - ', vv(1,1,kz)
      print *, '  IPAR=',ipar
      vcord='SGMA'
*SIGMA ONLY      vcord='PRES'
      parm='VREL'
      vcorda(ipar)=vcord
      parma(ipar)=parm
*      do 42 ii = kzs,kze
*         ftim = nint(mrf(1,6)/60.0)
*         iyr = mif(2,6)/100
*         ihr = mod(mif(2,6),100)*100
*         ilvl = nint(mrf(101+ii,6)*10000.0)
*         write(21,1581) iyr,'/',ihr,'F',ftim,ilvl,vcord,parm
*         write(21,1583) 'COLUMNS: 1 ',jy-2,'ROWS: 1 ',ix-2
*         write(21,1584) ' '
*         do 43 kk = ix-1,2,-1
*            write(21,1586) 'ROW ',kk-1
*            write(21,1587) ((vv(kk,jj,ii)),jj=2,jy-1)
* 43      continue
*         write(21,1584) ' '
* 42   continue
 
      ipar=ipar+1
* IPAR=3
      print *, 'Dumping W - ', ww(1,1,kz)
      print *, '  IPAR=',ipar
      vcord='SGMA'
*SIGMA ONLY      vcord='PRES'
      parm='WREL'
      vcorda(ipar)=vcord
      parma(ipar)=parm
*      do 78 ii = kzs,kze 
*         ftim = nint(mrf(1,6)/60.0)
*         iyr = mif(2,6)/100
*         ihr = mod(mif(2,6),100)*100
*         ilvl = nint(mrf(101+ii,6)*10000.0)
*         write(21,1581) iyr,'/',ihr,'F',ftim,ilvl,vcord,parm
*         write(21,1583) 'COLUMNS: 1 ',jy-2,'ROWS: 1 ',ix-2
*         write(21,1584) ' '
*         do 79 kk = ix-1,2,-1
*            write(21,1586) 'ROW ',kk-1
*            write(21,1587) (ww(kk,jj,ii),jj=2,jy-1)
* 79      continue
*         write(21,1584) ' '
* 78   continue
 
      ipar=ipar+1
* IPAR=4
      print *, 'Dumping Pres - ', prd(1,1,kz)
      print *, '  IPAR=',ipar
      vcord='SGMA'
*SIGMA ONLY      vcord='PRES'
      parm='PRES'
      vcorda(ipar)=vcord
      parma(ipar)=parm

      ipar=ipar+1
* IPAR=5
      print *, 'Dumping T - ', ttd(1,1,kz)
      print *, '  IPAR=',ipar
      vcord='SGMA'
*SIGMA ONLY      vcord='PRES'
      parm='TMPK'
      vcorda(ipar)=vcord
      parma(ipar)=parm

      ipar=ipar+1
* IPAR=6
      print *, 'Dumping Dewpoint - ', dewd(1,1,kz)
      print *, '  IPAR=',ipar
      vcord='SGMA'
*SIGMA ONLY      vcord='PRES'
      parm='DWPK'
      vcorda(ipar)=vcord
      parma(ipar)=parm

      ipar=ipar+1
* IPAR=7
      print *, 'Dumping dT/dZ - ', dtdz(1,1,kz)
      print *, '  IPAR=',ipar
      vcord='SGMA'
*SIGMA ONLY      vcord='PRES'
      parm='LAPS'
      vcorda(ipar)=vcord
      parma(ipar)=parm

      ipar=ipar+1
* IPAR=8
      print *, 'Dumping Cloud Water - ', clw(1,1,kz)
      print *, '  IPAR=',ipar
      vcord='SGMA'
*SIGMA ONLY      vcord='PRES'
      parm='CWTR'
      vcorda(ipar)=vcord
      parma(ipar)=parm
 
      ipar=ipar+1
* IPAR=9
      print *, 'Dumping Rain Water - ', rnw(1,1,kz)
      print *, '  IPAR=',ipar
      vcord='SGMA'
*SIGMA ONLY      vcord='PRES'
      parm='RWTR'
      vcorda(ipar)=vcord
      parma(ipar)=parm

*      do 118 ii = kzs,kze
*         ftim = nint(mrf(1,6)/60.0)
*         iyr = mif(2,6)/100
*         ihr = mod(mif(2,6),100)*100
*         ilvl = nint(mrf(101+ii,6)*10000.0)
*         write(21,1581) iyr,'/',ihr,'F',ftim,ilvl,vcord,parm
*         write(21,1583) 'COLUMNS: 1 ',jy-2,'ROWS: 1 ',ix-2
*         write(21,1584) ' '
*         do 119 kk = ix-1,2,-1
*            write(21,1586) 'ROW ',kk-1
*            write(21,1587) ((dewd(kk,jj,ii)),jj=2,jy-1)
* 119     continue
*         write(21,1584) ' '
* 118  continue

*      ipar=ipar+1
*      print *, 'Dumping Relative Humidity - ', rhd(1,1,kz)
**      vcord='SGMA'
*      vcord='PRES'
*      parm='RELH'
*      vcorda(ipar)=vcord
*      parma(ipar)=parm
**      do ii = kzs,kze
**         ftim = nint(mrf(1,6)/60.0)
**         iyr = mif(2,6)/100
**         ihr = mod(mif(2,6),100)*100
**         ilvl = nint(mrf(101+ii,6)*10000.0)
**         write(21,1581) iyr,'/',ihr,'F',ftim,ilvl,vcord,parm
**         write(21,1583) 'COLUMNS: 1 ',jy-2,'ROWS: 1 ',ix-2
**         write(21,1584) ' '
**         do kk =ix-1,2,-1
**            write(21,1586) 'ROW ',kk-1
**            write(21,1587) ((rhd(kk,jj,ii)),jj=2,jy-1)
**         end do
**         write(21,1584) ' '
**      end do
 
      ipar=ipar+1
* IPAR=10
      print *, 'Dumping Geopotential Height - ', ghtd(1,1,kz)
      print *, '  IPAR=',ipar
      vcord='SGMA'
*SIGMA ONLY      vcord='PRES'
      parm='HGHT'
      vcorda(ipar)=vcord
      parma(ipar)=parm
 
      ipar=ipar+1
* IPAR=11
      print *, 'Dumping surface Pressure'
      print *, '  IPAR=',ipar
      vcord='NONE'
      parm='PRES'
      vcorda(ipar)=vcord
      parma(ipar)=parm
*      ftim = nint(mrf(1,6)/60.0)
*      iyr = mif(2,6)/100
*      ihr = mod(mif(2,6),100)*100
*      ilvl = 0
*      write(21,1581) iyr,'/',ihr,'F',ftim,ilvl,vcord,parm
*      write(21,1583) 'COLUMNS: 1 ',jy-2,'ROWS: 1 ',ix-2
*      write(21,1584) ' '
*      do 46 kk = ix-1,2,-1
*         write(21,1586) 'ROW ',kk-1
*         write(21,1587) (sfpd(kk,jj),jj=2,jy-1)
* 46   continue
*      write(21,1584) ' '
 
      ipar=ipar+1
* IPAR=12
 3736 print *, 'Dumping sea level Pressure'
      print *, '  IPAR=',ipar
      vcord='NONE'
      parm='PMSL'
      vcorda(ipar)=vcord
      parma(ipar)=parm
*      ftim = nint(mrf(1,6)/60.0)
*      iyr = mif(2,6)/100
*      ihr = mod(mif(2,6),100)*100
*      ilvl = 0
*      write(21,1581) iyr,'/',ihr,'F',ftim,ilvl,vcord,parm
*      write(21,1583) 'COLUMNS: 1 ',jy-2,'ROWS: 1 ',ix-2
*      write(21,1584) ' '
*      do 48 kk = ix-1,2,-1
*         write(21,1586) 'ROW ',kk-1
*         write(21,1587) (slpd(kk,jj),jj=2,jy-1)
* 48   continue
*      write(21,1584) ' '
 
      ipar=ipar+1
* IPAR=13
      print *, 'Dumping Ground Temp - ', tgd(1,1)
      print *, '  IPAR=',ipar
      vcord='SGMA'
      parm='TMPK'
      vcorda(ipar)=vcord
      parma(ipar)=parm
*      ftim = nint(mrf(1,6)/60.0)
*      ilvl=10000
*      iyr = mif(2,6)/100
*      ihr = mod(mif(2,6),100)*100
*      write(21,1581) iyr,'/',ihr,'F',ftim,ilvl,vcord,parm
*      write(21,1583) 'COLUMNS: 1 ',jy-2,'ROWS: 1 ',ix-2
*      write(21,1584) ' '
*      do 85 kk = ix-1,2,-1
*         write(21,1586) 'ROW ',kk-1
*         write(21,1587) ((tgd(kk,jj)),jj=2,jy-1)
* 85   continue
*      write(21,1584) ' '
 
      ipar=ipar+1
* IPAR=14
      print *, 'Dumping 3 hr Precip - ', riad(1,1)
      print *, '  IPAR=',ipar
      vcord='NONE'
      parm='P03M'
      vcorda(ipar)=vcord
      parma(ipar)=parm
*      ftim = nint(mrf(1,6)/60.0)
*      ilvl=0
*      iyr = mif(2,6)/100
*      ihr = mod(mif(2,6),100)*100
*      write(21,1581) iyr,'/',ihr,'F',ftim,ilvl,vcord,parm
*      write(21,1583) 'COLUMNS: 1 ',jy-2,'ROWS: 1 ',ix-2
*      write(21,1584) ' '
*      do kk = ix-1,2,-1
*         write(21,1586) 'ROW ',kk-1
*         write(21,1587) ((riad(kk,jj)),jj=2,jy-1)
*      end do
*      write(21,1584) ' '
 
      ipar=ipar+1
* IPAR=15
      print *, 'Dumping 3 hr CONV Precip - ', ribd(1,1)
      print *, '  IPAR=',ipar
      vcord='NONE'
      parm='P3CN'
      vcorda(ipar)=vcord
      parma(ipar)=parm
*      ftim = nint(mrf(1,6)/60.0)
*      ilvl=0
*      iyr = mif(2,6)/100
*      ihr = mod(mif(2,6),100)*100
*      write(21,1581) iyr,'/',ihr,'F',ftim,ilvl,vcord,parm
*      write(21,1583) 'COLUMNS: 1 ',jy-2,'ROWS: 1 ',ix-2
*      write(21,1584) ' '
*      do kk = ix-1,2,-1
*         write(21,1586) 'ROW ',kk-1
*         write(21,1587) ((riad(kk,jj)),jj=2,jy-1)
*      end do
*      write(21,1584) ' '
 
      ipar=ipar+1
* IPAR=16
      print *, 'Dumping Terrain - ', terd(1,1)
      print *, '  IPAR=',ipar
      vcord='NONE'
      parm='HGHT'
      vcorda(ipar)=vcord
      parma(ipar)=parm
*      ftim = nint(mrf(1,6)/60.0)
*      iyr = mif(2,6)/100
*      ihr = mod(mif(2,6),100)*100
*      ilvl = 0
*      write(21,1581) iyr,'/',ihr,'F',ftim,ilvl,vcord,parm
*      write(21,1583) 'COLUMNS: 1 ',jy-2,'ROWS: 1 ',ix-2
*      write(21,1584) ' '
*      do 47 kk = ix-1,2,-1
*         write(21,1586) 'ROW ',kk-1
*         write(21,1587) (terd(kk,jj),jj=2,jy-1)
* 47   continue
*      write(21,1584) ' '
*
 
      ipar=ipar+1
* IPAR=17
      print *, 'Dumping Low Cloud Amount - ', vqcl(1,1)
      print *, '  IPAR=',ipar
      vcord='PRES'
      parm='CLDW'
      vcorda(ipar)=vcord
      parma(ipar)=parm
*

      ipar=ipar+1
* IPAR=18
      print *, 'Dumping Middle Cloud Amount - ', vqcm(1,1)
      print *, '  IPAR=',ipar
      vcord='PRES'
      parm='CLDW'
      vcorda(ipar)=vcord
      parma(ipar)=parm
*

      ipar=ipar+1
* IPAR=19
      print *, 'Dumping High Cloud Amount - ', vqch(1,1)
      print *, '  IPAR=',ipar
      vcord='PRES'
      parm='CLDW'
      vcorda(ipar)=vcord
      parma(ipar)=parm
*
*      call mm5gem(iyr,ihr,ftim,ilvla,vcorda,parma,jy,ix,kz,
*     + uu,vv,wwd,ttd,dewd,ghtd,clw,rnw,dtdz,prd,
*     + vqcl,vqcm,vqch,sfpd,slpd,tgd,riad,ribd,terd,utot,
*     + tgarea,tproj,tkxky,fdatim,ixl,jyl,kzl,ipar,itime,iflno,
*     + idom)

*
*
*
      iwc=66
      do iwrite=1,izn      ! loop over all requested WOCSS zones
        iwc=iwc+1
        xmnla=zmnla(iwrite)
        xmxla=zmxla(iwrite)
        xmnlo=zmnlo(iwrite)
        xmxlo=zmxlo(iwrite)
*
      if(iwrite.ne.0)then   ! if text files are to be written
        alon0=mrf(3,1)
*&&&        tlat=30.0
* cone factor
        tlat=0.7155669
        if(iwrite.gt.0)then
          write(*,*)'  $   $   $   $   $   $   $   $   $   $   $  '
          write(*,*)'       MM5 ASCII files being created         '
          write(*,*)'  $   $   $   $   $   $   $   $   $   $   $  '
*
        endif
 1101 format(5x,'MM5 output for ',a14)
* 1103 format('12345678901234567890123456789012345678901234567890',
*     + '123456789012345678901234')
 1103 format('     LAT     LON  V(mi/h)      DIR  T(C)    Qv    ',
     + 'Qc    Qr    Z(m)   P(mb)')
 1105 format(2f8.2,2f9.2,4f6.2,2f8.2)
 1107 format(16x,2f9.2,4f6.2,2f8.2)
        ict=0
        do 2710 i=1,ix
        do 2710 j=1,jy
          aa = min(i,ix-1)
          bb = min(j,jy-1)
          cc = max(i-1,1)
          dd = max(j-1,1)
          xlt = 0.25*(xlat(aa,bb)+xlat(cc,bb)
     &              +xlat(aa,dd)+xlat(cc,dd))
          xln = 0.25*(xlon(aa,bb)+xlon(cc,bb)
     &              +xlon(aa,dd)+xlon(cc,dd))
*
          if(qvtot(i,j).le.1.e-03)goto 2710
*
          do 2705 kk=kz,1,-1
            ppd = 0.25*(pp(aa,bb,kk)+pp(cc,bb,kk)
     &                +pp(aa,dd,kk)+pp(cc,dd,kk))
            ppmb=pstd(i,j)*sih(kk)+mrf(1,2)+ppd
            u = uu(i,j,kk)
            v = vv(i,j,kk)
* meters per second
            vspdms = sqrt(u*u + v*v)
* convert to statute miles per hour
            vspd = vspdms*(3600.)*(39.37/(12.*5280.))
* convert to international nautical miles per hour
*            vspd = vspd*(3600.)*(39.37/(12.*6076.12))
            call unrotate(u,v,xln,alon0,tlat,dir)
            tcels = ttd(i,j,kk)-273.15 
            tgcels = tgd(i,j)-273.15
            dewcls = dewd(i,j,kk)-273.15
            tfg = (tgcels*9.0)/5.0 + 32.0
* fill CMMC arrays
            if(kk.eq.kz)then
             xcm(i,j,1)=u
             xcm(i,j,2)=v
             xcm(i,j,3)=ttd(i,j,kk)
             xcm(i,j,4)=rhd(i,j,kk)
             xcm(i,j,5)=riad(i,j)+ribd(i,j)
             xcm(i,j,6)=xlt
             xcm(i,j,7)=xln
             xcm(i,j,8)=terd(i,j)
            endif
            x3dm(i,j,kk,1)=u
            x3dm(i,j,kk,2)=v
            x3dm(i,j,kk,3)=wwd(i,j,kk)
            x3dm(i,j,kk,4)=ttd(i,j,kk)
            x3dm(i,j,kk,5)=rhd(i,j,kk)
            x3dm(i,j,kk,6)=ppmb
            x3dm(i,j,kk,7)=ghtd(i,j,kk)
*
            if((xlt.ge.xmnla).and.(xlt.le.xmxla).and.
     +        (xln.ge.xmnlo).and.(xln.le.xmxlo))then
                if(kk.eq.kz)then
*>>>>>>>>>>>>>>>>>> write WOCSS "obs" sfc variables here . . .
                  xlt0=xlt
                  xln0=xln
                  call utmclc(xlt0,xln0,utme,utmn)
*
* reduce winds at kk=kz to surface using LKB (JAS, 1979) model
*
* LKB input
                  uspd = vspdms
                  t = tcels
                  relh = rhd(i,j,kk)/100.
                  xmrat = xmr(ppmb,ttd(i,j,kk),relh)
                  q = xmrat/(xmrat+1.0)
                  tsfc = tgcels
                  zu = ghtd(i,j,kk)-terd(i,j)
                  zt = ghtd(i,j,kk)-terd(i,j)
                  zq = ghtd(i,j,kk)-terd(i,j)
                  id = 1
                  p = ppmb
*debug       write(*,31769)uspd,t,relh,xmrat,q
*debug       write(*,31769)tsfc,zu,zt,zq,p
*debug31769  format(5e15.7)
                  call lkb79f(ier)
* compute U_5 (5 meter) using LKB output
                  puz = psi(1,zl)
                  u5 = usr*((alog(5./zo)-puz)/(0.4))
                  ulvl = usr*((alog(zu/zo)-puz)/(0.4))
                  ddir = dir
*debug       write(*,31769)zl,puz,uspd,ulvl,u5
                  if((u5.lt.0.).or.(ier.gt.0))then
                    ddir=0.
                    u5=0.
                    if(ier.gt.0)then
         write(*,4055)uspd,t,q,tsfc
 4055    format(' LKB fails for uspd,t,q,tsfc=',2f6.2,e15.7,f6.2)
         write(*,*)' IER=',ier,'<<<<<<<<<<<<<<<<<<<<'
*
*  missing wind flag, set wind speed=999.0 (see GEOSIG in wocss)
                      u5=999.0
                      ddir=999.0
                    endif
                  endif
                  if(u5.lt.0.05)ddir=0.
                  ict=ict+1
                  uutme(ict)=utme
                  uutmn(ict)=utmn
                  telv(ict)=terd(i,j)
                  xltme(ict)=xlt
                  xlnmn(ict)=xln
*OLD*                  write(iwc,3711)utme,utmn,sfpd(i,j),tfg,ddir,u5
*OLD                  write(iwc,3711)utme,utmn,slpd(i,j),tfg,ddir,u5
*OLD 3711             format(f6.1,f8.1,f8.1,f5.0,f7.1,f8.1)
                  if(ict.eq.1)then
                   write(iwc,3705)
 3705              format('sta      UTM east UTM north   elev m.  ',
     +             'press hP    T deg C   dir deg   spd m/s  latitude  ',
     +             'longitud    T deg F  prss msl  altim in')
                  endif
                   write(67,3711)'MMGRD',utme,utmn,terd(i,j),sfp(i,j),
     +             tgcels,ddir,u5,xlt,xln,tfg,slp(i,j),-9999.0
 3711              format(a5,12f10.2)
                  if(ict.gt.nwsite)then
                    write(*,*)' '
                    write(*,*)'  actual number of MM5 sites in '
                    write(*,*)'  WOCSS area exceeds variable   '
                    write(*,*)'  "nwsite", re-define. STOP!'
                    write(*,*)'  ict,nwsite=',ict,nwsite
                    STOP 67101
                  endif
                  vert(ict,kk,1)=ghtd(i,j,kk)
                  vert(ict,kk,2)=dir
                  vert(ict,kk,3)=vspdms
                  vert(ict,kk,4)=tcels
                  vert(ict,kk,5)=ppmb
                else     ! if(kk.eq.kz)then
*>>>>>>>>>>>>>>>>>> fill WOCSS "obs" upper-air arrays here . . .
* convert geoht(m) to geoht(ft) geohtft=geohtm/(0.3048) for WIND prof
* keep geoht(m) same                                    for TEMP prof
                  if(ict.gt.nwsite)then
                    write(*,*)' '
                    write(*,*)'  actual number of MM5 sites in '
                    write(*,*)'  WOCSS area exceeds variable   '
                    write(*,*)'  "nwsites", re-define. STOP!'
                    write(*,*)'  ict,nwsite=',ict,nwsite
                    STOP 67101
                  endif
                  vert(ict,kk,1)=ghtd(i,j,kk)
                  vert(ict,kk,2)=dir
                  vert(ict,kk,3)=vspdms
                  vert(ict,kk,4)=tcels
                  vert(ict,kk,5)=ppmb
                endif     ! if(kk.eq.kz)then
            endif      ! if((xlt.ge.xmnla).and.(xlt.le.xmxla).and...
 2705     enddo
 2710   enddo
 2701   format(1500(8e15.7,/))
        if(ix*jy.gt.1500*8)then
          write(*,*)' re-format statement number 2701, STOP'
          STOP 2799
        endif
*>>>>>>>>>>>>>>>> write WOCSS "obs" file here .  .  .
* convert geoht(m) to geoht(ft) geohtft=geohtm/(0.3048) for WIND prof
* keep geoht(m) same                                    for TEMP prof
        write(*,*)' '
        write(*,*)' total # of MM5 gridpoints in WOCSS area=',ict
        write(*,*)' '
        do ii=1,ict
*OLDW             write(iwc,*)' '
*OLDW             write(iwc,6741)uutme(ii),uutmn(ii)
*OLDW             write(iwc,6751)kz
             write(iwc,*)' '
             write(iwc,6741)uutme(ii),uutmn(ii),telv(ii),xltme(ii),
     +                      xlnmn(ii)
             write(iwc,6751)kz,'MMVRT'
             write(iwc,6752)
 6741        format(5f10.2)
 6751        format(2x,i10,2x,a5)
 6752        format('    ht-m','    dir.',' spd-m/s')
          do kk=kz,1,-1
*OLDW             write(iwc,6761)vert(ii,kk,1)/0.3048,vert(ii,kk,2),
*OLDW     +       vert(ii,kk,3)
             write(iwc,6761)nint(vert(ii,kk,1)),nint(vert(ii,kk,2)),
     +       vert(ii,kk,3)
 6761        format(2x,i6,2x,i6,2x,f6.1)
          enddo
        enddo
        do ii=1,ict
*OLDW             write(iwc,*)' '
*OLDW             write(iwc,6741)uutme(ii),uutmn(ii)
             write(iwc,*)' '
             write(iwc,6741)uutme(ii),uutmn(ii),telv(ii),xltme(ii),
     +                      xlnmn(ii)
*OLDW             write(iwc,6753)kz,2
             write(iwc,6753)kz,2,'MMVT2'
             write(iwc,6754)
 6753        format(2(2x,i6),2x,a5)
 6754        format('    ht-m','    temp.-C','      pr-mb')
          do kk=kz,1,-1
*OLDW             write(iwc,6761)vert(ii,kk,1),vert(ii,kk,4),
*OLDW     +       vert(ii,kk,5)
             write(iwc,6763)nint(vert(ii,kk,1)),vert(ii,kk,4),
     +       vert(ii,kk,5)
 6763        format(2x,i6,5x,f6.1,4x,f7.2)
          enddo
        enddo
*
      endif   ! if text files are to be written
*
      enddo   ! loop over all requested WOCSS zones
CCCCC
C return for next time
CCCCC
      goto 30
 101  print *, 'End of File'
*
      itime=-999
*      call mm5gem(iyr,ihr,ftim,ilvla,vcorda,parma,jy,ix,kz,
*     + uu,vv,wwd,ttd,dewd,ghtd,clw,rnw,dtdz,prd,
*     + vqcl,vqcm,vqch,sfpd,slpd,tgd,riad,ribd,terd,utot,
*     + tgarea,tproj,tkxky,fdatim,ixl,jyl,kzl,ipar,itime,iflno,
*     + idom)

      do jj=1,izn
        iwc = 66+jj
        close(iwc)
      enddo
      stop
      end
      subroutine ghtsig(ix,jy,kz)
*
      parameter(ixm=140,jym=140,kzm=50)
*
      integer iyr, ihr, ilvl, ii, jj, kk, aa,bb,cc,dd,i,j,k
      integer ftim, kzs, kze, ix, jy, kz
      real uu(ixm,jym,kzm), vv(ixm,jym,kzm), junk3(ixm,jym,kzm),
     + tt(ixm,jym,kzm), rh(ixm,jym,kzm), pp(ixm,jym,kzm), 
     + ght(ixm,jym,kzm), qc(ixm,jym,kzm), qr(ixm,jym,kzm),
     + sfp(ixm,jym), slp(ixm,jym), pmb(kzm),
     + pstx(ixm,jym), pstd(ixm,jym), ter(ixm,jym), tg(ixm,jym),
     + sih(kzm), mrf(1000,20)
      real wmixrat,tvll, beta, gpll, tvavg, q, t, p, e
      real refrat, yllc, yurc, xllc, xurc, xlatll, xlatur,
     +xlonll, xlonur
      integer iahr, ibhr, output_hr, oahr, filnum, ftime, mdate,
     +domid, fhour
*
      real tp1(ixm,jym,kzm), tp2(ixm,jym,kzm), tp3(ixm,jym,kzm),
     + tp4(ixm,jym,kzm), tp5(ixm,jym,kzm), tp6(ixm,jym,kzm),
     + tp7(ixm,jym,kzm)
*
      real xnsig(ixm,jym,8)
*
      integer itopx(ixm,jym), ibotx(ixm,jym)
      integer itopd(ixm,jym), ibotd(ixm,jym)
*
      common/arrs/uu,vv,junk3,tt,rh,pp,ght,qc,qr,sfp,slp,pstx,pstd,
     + ter,tg,mrf,sih,pmb
*
* Compute geopotential height for sigma surfaces
*
      eps = 0.622
      do 12000 j=1,jy-1
         do 12000 i=1,ix-1
          psfc = sfp(i,j)
*          kbot=kz-1
          kbot=kz
          ktop=1
*
* Compute geopotential height for isobaric surfaces
         do 11000 k=kbot,ktop,-1
          p=pstx(i,j)*sih(k)+mrf(1,2)+pp(i,j,k)
          t=tt(i,j,k)
          if(t.ne.-9999.0)then
            if(k.eq.kbot)then
              p=pstx(i,j)*sih(kz)+mrf(1,2)+pp(i,j,kz)
              relh = rh(i,j,kz)
              t=tt(i,j,kz)
              tcels=t-273.15
              esat=6.112*exp((17.67*tcels)/(tcels+243.5))
              qsat=eps*esat/p 
              q=(relh/100.)*qsat
              tvll=tt(i,j,kz) * (1.+0.608*q)
              beta=287.04/9.81* log(psfc/p)
              ght(i,j,kz)=ter(i,j)+beta*tvll/(1.-.5*beta*0.0065)
***********************
*         if((i.eq.43).and.(j.eq.27))then
*           write(*,*)' k,ter(i,j),beta,tvll,psfc,p,ght(i,j,k)=',
*     +                k,ter(i,j),beta,tvll,psfc,p,ght(i,j,k)
*         endif
***********************
              p0=p
              if((t.lt.0.).or.(relh.lt.0.))then
                write(*,*)' ERROR in computing GEOPHT'
                write(*,*)i,j,k,t,relh
                STOP 8764
              endif
            else
              relh0 = rh(i,j,k+1)
              t0=tt(i,j,k+1)
              tcels=t0-273.15
              esat0=6.112*exp((17.67*tcels)/(tcels+243.5))
              qsat0=eps*esat0/p0
              q0=(relh0/100.)*qsat0
              tv0=t0 * (1.+0.608*q0)
              relh = rh(i,j,k)
              t=tt(i,j,k)
              tcels=t-273.15
              esat=6.112*exp((17.67*tcels)/(tcels+243.5))
              qsat=eps*esat/p
              q=(relh/100.)*qsat
              tv=t * (1.+0.608*q)
              beta=287.04/9.81* log(p0/p)
              tvavg = (tv0+tv)/2.
              ght(i,j,k)=beta*tvavg + ght(i,j,k+1)
***********************
*         if((i.eq.43).and.(j.eq.27))then
*           write(*,*)' k,ter(i,j),beta,tvavg,p,p0,ght(i,j,k)=',
*     +                k,ter(i,j),beta,tvavg,p,p0,ght(i,j,k)
*         endif
***********************
              p0=p
              if((t0.lt.0.).or.(relh0.lt.0.))then
                write(*,*)' ERROR in computing GEOPHT'
                write(*,*)i,j,k,t0,relh0
                STOP 8774
              endif
            endif
          else 
            ght(i,j,k)=-9999.0
          endif
11000 continue
*
12000 continue
*
      return
      end
      subroutine unrotate(u,v,xlon,alon0,tlat,dir)
c
c  This routine takes LCC map projection u,v winds and rotates them
c    into true earth relative u,v winds
c
c "tlat" has cone factor [2 October 2001, D. Miller]
c
      dir=0.0
      pi=acos(-1.)
      deg2r=pi/180.0
*      tlat=25.0
*&&&      an=cos((90.-tlat)*deg2r)
      ue=u
      ve=v
c
c  Find angle between longitude at this location and north for LCC map
c
      dlon=alon0-xlon
      if(dlon.gt.180.0)dlon=dlon-360.0
      if(dlon.lt.-180.0)dlon=dlon+360.0
*&&&      angle=dlon*an*pi/180.0
      angle=dlon*tlat*pi/180.0
c
c  Calculate map wind components
c
      u=-ve*sin(angle)+ue*cos(angle)
      v=ve*cos(angle)+ue*sin(angle)
      vspd = sqrt(u*u + v*v)
      if(vspd.gt.1.e-08)then
c
c determine direction wind is heading TOWARD
c
        if(u.lt.0.)then
          ang=acos(u/vspd)
          if(v.lt.0.)ang=-pi-asin(v/vspd)
        else
          ang=asin(v/vspd)
        endif
        if(ang.lt.0.0)ang=ang+2.*pi
        ang=ang/deg2r
        if((ang.ge.0.0).and.(ang.lt.90.))then
          dir=90.0-ang
        else
          dir=450.0-ang
        endif
c
c determine direction wind is heading FROM
c
        if(dir.gt.180.)then
          dir=dir-180.
        else
          dir=dir+180.
        endif
      endif
c
      return
      end
      function nblank(cline)
*
      character cline*100
*
      integer nblank
*
      ifnd=0
      do 2000 ii=100,1,-1
*
        if(ifnd.eq.0)then
          if(cline(ii:ii).ne.' ')ifnd=ii
        endif
*
 2000 continue
*
      nblank=ifnd
      return
      end
