#!/bin/ksh 
# Author:        Geoff Manikin       Org: NP22         Date: 2007-08-06
#
# Script history log:
#======================================================================
#  Set Defaults fcst hours,cycle,model,region in config_nam_nwpara called in parent job

# RUNTYP: OUTPUT REGION TO DOWNSCALE TO  (IN SMINIT.CTL FILE)
#=================================================================
# conus        : Downscale NAM 12 over CONUS  
#              :  SREF-GRD=212  NAM-GRD=bgrd  NDFD-GRD=197
# pr           :  SREF-GRD=212  NAM-GRD=bgrd  NDFD-GRD=195
# hi           :  SREF-GRD=243  NAM-GRD=bgrd  NDFD-GRD=196
# ak           :  SREF-GRD=216  NAM-GRD=bgrd  NDFD-GRD=198
# ak_rtmages   :  " " forecast hours 0-12
# conusnest    :  SREF-GRD=212   NAM-GRD=conusnest.bsmart      NDFD-GRD=197
# conusnest2p5 :  SREF-GRD=212   NAM-GRID=conusnest.bsmart     NDFD-GRD=184/187
# priconest    :  SREF=GRD=212   NAM-GRD= prinest.bsmart       NDFD-GRD=195
# hawaiinest   :  SREF-GRID=243  NAM-GRID=hawaiinest.bsmart    NDFD-GRD=196  
# alaskanest   :  SREF-GRID=216  NAM-GRID=alaskanest.bsmart    NDFD-GRD=198  
# aknest3      :  SREF-GRID=216  NAM-GRID=alaskanest.bsmart    NDFD-GRD=91

# guamnmmb     :  GEFS-GRID???   HRW-GRID=guamnmmb.t00z.wrfprs NDFD-GRD=199
# guamarw      :  GEFS-GRID???   HRW-GRID=guamarw.t00z.wrfprs  NDFD-GRD=199

# dgex_cs      :  SREF-GRID=212  DGEXGRID=dgex_conus.tCCz.bsmart  NDFD-GRD=184
# dgex_ak      :  SREF-GRID=216  DGEXGRID=dgex_alaska.tCCz.bsmart NDFD-GRD=91
#======================================================================

set -x
inest=`echo $RUNTYP|awk '{ print( index($0,"nest") )}' `
echo inest=$inest
# Define core (nmmb, arw, nems) needed for hiresw veg initialization
icore=`echo $RUNTYP|awk '{ print( index($0,"nmmb") )}' `
echo icore=$icore
if [ $icore -eq 0 ];then
  icore=`echo $RUNTYP|awk '{ print( index($0,"arw") )}' `
fi
core=GFS 
if [ $icore -gt 0 ];then
  core=`echo $RUNTYP |cut -c $icore-`
fi
export rg=`echo $RUNTYP |cut -c1-2` 
tempvar=$(echo EXEC$mdl)
EXECmdl=$(eval echo \$$tempvar)

#=====================================================================
# Set special filename extensions for mdl,sref,master,wgt,output files
# mdl input file         : mdlgrd,natgrd
# eg: nam.t12z.conusnest.bsmart.tm00

# prdgen master ctl file : RUNTYPE
# eg: nam_smartmasteraknest3.ctl     

# prdgen wgt ctl file    : mdlgrd, ogrd
# eg: nam_wgt_187_conusnest

# smartinit in/out file  : rg / outreg
# eg: MESOAK.NDFD
# eg: nam.t12z.smartconus2p5.f03
#=====================================================================

# READ IN GRID INFO
linemax=`cat SMINIT.CTL |wc -l`
echo SMINIT  $linemax
let iline=0
while [ $iline -le $linemax ];do
  head -n $iline SMINIT.CTL >tempfile
  line=`tail -n1 tempfile`
  let k=1
  for word in $line; do
    case $k in
      1)  RCFG=$word;;
      2)  export rg=$word;;
      3)  export outreg=$word;;
      4)  export mdlgrd=$word;;
      5)  export sgrb=$word;;
      6)  export natgrd=$word;;
      7)  export ogrd=$word;;
      8)  gtyp=$word;;
      9)  gnx=$word;;
     10)  gny=$word;;
     11)  glat1=$word;;
     12)  glon1=$word;;
     13)  gdt=$word;;
     14)  tlon=$word;;
     15)  dx=$word;;
     16)  dy=$word;;
    esac
    let k=k+1
  done
  if [ $RUNTYP = "$RCFG" ];then
    if [ "$gtyp" = "$ogrd" ];then 
      export grid=$ogrd
    else
      export grid="$gtyp $gnx $gny $glat1 $glon1 $gdt $tlon $dx $dy"
    fi
    break
  else
    let iline=iline+1
    if [ $iline -gt $linemax ];then
      echo;echo  $RUNTYP not found in SMINIT.CTL file
      echo  EXITING SMARTINIT; exit
    fi
  fi
done
#case $rg in dgx) inest=1;;esac  #set to read in 6hr precip for dgex files

#set -x

prdgfl=meso${rg}.NDFD  # output prdgen grid name (eg: mesocon.NDFD,mesoak...)
case $RUNTYP in
 guamnest ) prdgfl=$mdl.t${cyc}z.smartinitin.guam;;
    conus ) prdgfl=meso.NDFD;;    
  aknest3 ) prdgfl=mesoak.NDFD;;  # CHANGE should be mesoak3.NDFD (meso{rg}
esac

cycon=0
case $cyc in 
   00|12 ) cycon=1;; 
esac

# Set forecast hours to compute 12 hour max/min Temps and 12 hr accum precip
# 12 hour max/mins must be computed at 00 and 12 UTC

# FOR NESTS,parent script, exnam, sets forecast range (60 or 54h)
case $cyc in
  00|12) set -A A6HR 12 24 36 48 60 72 84 96 108 120 132 144 156 168 180 192;;
  * )    set -A A6HR 18 30 42 54 66 78 90 102 114 126 138 150 162 174 186;;
esac
# srefcyc and gefscyc set in parent job (JNAM_SMINIT)
#typeset -Z2 srefcyc gefscyc pcphrl

#======================================================================
#  Configure input met grib, land-sea mask and topo file names
#======================================================================

#  Set indices to determine input met file name 
#       eg: MDL.tCYCz.MDLGRD.NATGRD${FHR}.tm00
#       eg: nam.t12z.bgrd3d24.tm00
#       eg: nam.t12z.conusnest.bsmart24.tm00

#-------------------------------------------------------------------------
#   For all grids, set the following in NAM_SMINIT.CTL:
#   sgrb : Input SREF grid grib number (eg: 212, 216, 243)
#   grid : output grid to copygb sref precip and nam precip buckets to 
#          one exception for non-nests where nam precip buckets are 
#          interpolated to smartinit output (ogrd)
#   ogrd : output grib number for prdgen and smartinit codes 
#          (eg: 197,196,195,198,184)
#--------------------------------------------------------------------------
if [ $gtyp -ne $ogrd ];then
case $RUNTYP in
  ak|ak_rtmages|aknest3) grid="255 $grid  0 64 0 25000 25000";;
                      *) grid="255 $grid  0 64 2500 2500";;
esac
fi

# Set NDFD output grid topo and land mask filenames
maskpre=${mdl}_smartmask${outreg}
topopre=${mdl}_smarttopo${outreg}
ext=grb
maskfl=${maskpre}.${ext}
topofl=${topopre}.${ext}

echo
echo "============================================================================"
echo BEGIN SMARTINIT PROCESSING FOR FFHR $ffhr  CYCLE $cyc
echo RUNTYP:  $RUNTYP mdlgrd: $mdlgrd  rg: $rg
echo INTERP GRID: $grid
echo OUTPUT GRID: $ogrd $outreg
echo "============================================================"
echo 

#  Set Defaults pcp hours and frequencies
let pcphr=ffhr+3
let pcphrl=ffhr+3
let pcphr12=pcphr-12
let pcphr6=pcphr-6
let pcphr3=pcphr-3

let ffhr1=ffhr-1
let ffhr2=ffhr-2
hours="${ffhr}"

#===========================================================
#  CREATE Accum precip buckets if necessary 
#===========================================================
for fhr in $hours; do
  rm -f *out${fhr}
  mk3p=0;mk6p=0;mk12p=0
  let check3=fhr%3
  let check6=fhr%6
  let check12=fhr%12
  echo check3=$check3 check6=$check6 check12=$check12
  let fhr1=fhr-1
  let fhr2=fhr-2
  let fhr3=fhr-3
  let fhr6=fhr-6
  let fhr9=fhr-9
# typeset -Z2 fhr1 fhr2 fhr3 fhr6 fhr9 fhr ffhr 
echo FHR=$fhr FHR1=$fhr1 FHR2=$fhr2 FHR3=$fhr3 FHR6=$fhr6 FHR9=$fhr9  
if [ $fhr -gt 00 ];then 
 if [ $fhr -lt 10 -a $check3 -ne 0 ];then fhr="0"${fhr};fi
 if [ $fhr1 -lt 10 ];then fhr1="0"${fhr1};fi
 if [ $fhr2 -lt 10 ];then fhr2="0"${fhr2};fi
 if [ $fhr3 -lt 10 ];then fhr3="0"${fhr3};fi
 if [ $fhr6 -lt 10 ];then fhr6="0"${fhr6};fi
 if [ $fhr9 -lt 10 ];then fhr9="0"${fhr9};fi
fi
if [ $fhr -lt 10 ] ;then
  typeset -Z2 fhr1 fhr2 fhr3 fhr6 fhr9 fhr ffhr
fi
# Check that 00 hr analysis is from NDAS or GDAS
    case $natgrd in 
      initin) 
#     Check that  00 hr analysis is from NDAS or GDAS (08/2013)
          if [ $mdl = "gfs" ]; then
            export OUTTYP=3
            export PARMGLOBAL=$ROOTdng/parm
            export SIGINP=$COMIN/$mdl.t${cyc}z.sf$fhr
            export CTLFILE=$PARMdng/${mdl}_downscale_cntrl.parm
            export VDATE=`${utilexec}/ndate +${fhr} ${PDY}${cyc}`
            export FLXINP=flxfile
            export PGBOUT=tmpfile4
            export FILTER=0
            export SFCINP=$COMIN/$mdl.t${cyc}z.bf$fhr
            export IGEN=89
            export IDRT=0
            export LONB=1440
            export LATB=721
            export grid='255 0 1440 721 90000 0 128 -90000 359750 250 250 0'
            $utilexec/copygb -g "$grid" -x $COMIN/$mdl.t${cyc}z.sfluxgrbf$fhr $FLXINP
            export POSTGPEXEC=/global/save/emc.glopara/svn/post/tags/post_upgrade_gfs_2014_v6/src/ncep_post
	    export POSTGPSH=/global/save/emc.glopara/svn/post/tags/post_upgrade_gfs_2014_v6/ush/global_nceppost.sh 
	    $POSTGPSH > post${fhr}.out
            grid="255 1 193 193 12350 143687 128 16794 148280 20000 0 64 2500 2500"
            $utilexec/copygb -g "$grid" -x tmpfile4 $mdl.t${cyc}z.smartinitin.guam.${fhr}
            $utilexec/grbindex $mdl.t${cyc}z.smartinitin.guam.${fhr} $mdl.t${cyc}z.smartinitin.guam.${fhr}.idx
          fi
          inhrfrq=1
    esac
  if [ $fhr -gt ${fhrstr} ];then

#-------------------------------------------------------------
#   OFF-CYC & Nests: Create 6/12 hour buckets, 3 hr buckets available
#   ON-CYC :
#     3hr precip available at only 3,15, 27,39... forcast fhours
#     other hours, create 3 hr precip
#     6hr precip: Create only at 00/12 UTC valid times 
#           eg: fhr=12,24,36
#     Create 12 hour precip at 00/12 UTC valid times
#-------------------------------------------------------------
    if [ $check6 -eq 0 -o $check12 -eq 0 ];then #True at 6,12,18
       mk3p=3
       ppgm=make
    fi
#   hr3bkt flag determines when to run smartprecip to create 3 hr buckets
    let hr3bkt=$((fhr-3))%12
    echo hr3bkt=$hr3bkt 

#-------------------------------------------------------------
#   ON-CYC: The 3-hr fhrs (3,9,15,21,27,39....)--> Already have 3-hr buckets
# Create 3-hr buckets at 6,12,18,24,...
#-------------------------------------------------------------
#-------------------------------------------------------------
# ON-CYCLE:  At 12-hr times:  Need 6 hour buckets as well
# Except for 6 hr times (18,30,42...) : Already have 6 hour buckets
# In addition, For 00/12 UTC valid times: Need to make 12 hour accumulations
#-------------------------------------------------------------
  case $fhr in 
    ${A6HR[0]}|${A6HR[1]}|${A6HR[2]}|${A6HR[3]}|${A6HR[4]}|${A6HR[5]}|${A6HR[6]}| \
    ${A6HR[7]}|${A6HR[8]} )
      mk3p=3
      mk12p=12
  esac 

  echo MKPCP Flags: MK3P=$mk3p   MK6P=$mk6p   MK12P=$mk12p
  for MKPCP in $mk3p $mk6p $mk12p;do
    if [ $MKPCP -ne 0 ];then
    echo ====================================================================
    echo BEGIN Making $MKPCP hr PRECIP Buckets for fhr=$fhr and ppgm=$ppgm 
    echo ====================================================================
      pfhr3=-99;pfhr4=-99
      case $MKPCP in
        $mk3p )
        FHRFRQ=$fhr3;freq=3
        pfhr1=$fhr;pfhr2=$fhr3
        ppgm=make;;

        $mk6p )
        FHRFRQ=$fhr6;freq=6
        pfhr1=$fhr;pfhr2=$fhr6;;

        $mk12p )
        FHRFRQ=$fhr6;freq=12
        pfhr1=$fhr6;pfhr2=$fhr
        ppgm=add;;
      esac
  
     echo FHRFRQ=$FHRFRQ and freq=$freq
     grid199="10 6 0 0 0 0 0 0 193 193 12350000 143687000 48 20000000 16794000 148280000 64 0 2500000 2500000"
     $utilexec/copygb2 -g "$grid199" -i0 -x -k '8 1 8 2 0 96 0 0 1' $COMIN/gfs.t${cyc}z.master.grb2f${FHRFRQ} ${FHRFRQ}apcpfile.$fhr
     $utilexec/copygb2 -g "$grid199" -i0 -x -k '8 1 10 2 0 96 0 0 1' $COMIN/gfs.t${cyc}z.master.grb2f${FHRFRQ} ${FHRFRQ}acpcpfile.$fhr
     $utilexec/copygb2 -g "$grid199" -i0 -x -k '0 1 13 2 0 96 0 0 1' $COMIN/gfs.t${cyc}z.master.grb2f${FHRFRQ} ${FHRFRQ}weasdfile.$fhr
     cat ${FHRFRQ}apcpfile.$fhr ${FHRFRQ}acpcpfile.$fhr ${FHRFRQ}weasdfile.$fhr > WRFPRS${FHRFRQ}.tm00.grb2
     $utilexec/cnvgrib -g21 WRFPRS${FHRFRQ}.tm00.grb2 WRFPRS${FHRFRQ}.tm00
     $utilexec/grbindex WRFPRS${FHRFRQ}.tm00 WRFPRS${FHRFRQ}i.tm00

    export pgm=smartprecip; #. prep_step
    ln -sf "WRFPRS${FHRFRQ}.tm00"  fort.13  
    ln -sf "WRFPRS${FHRFRQ}i.tm00" fort.14
    ln -sf "$mdl.t${cyc}z.smartinitin.guam.${fhr}"      fort.15
    ln -sf "$mdl.t${cyc}z.smartinitin.guam.${fhr}.idx"  fort.16
    ln -sf "${freq}precip.${fhr}"  fort.50
    ln -sf "${freq}cprecip.${fhr}" fort.51
    ln -sf "${freq}snow.${fhr}"    fort.52

    if [ $MKPCP -eq $mk12p ];then
      ln -sf "WRFPRS${fhr6}.tm00"      fort.13    
      ln -sf "WRFPRS${fhr6}i.tm00"     fort.14
      ln -sf "$mdl.t${cyc}z.smartinitin.guam.${fhr}"       fort.15
      ln -sf "$mdl.t${cyc}z.smartinitin.guam.${fhr}.idx"   fort.16
    fi  # mk12p

#===============================================================
# GFS DNG now uses nam_smartprecip : Create Precip Buckets for smartinit 
#===============================================================
    echo RUN SMARTPRECIP FOR GFS DNG TO MAKE $freq HR PRECIP BUCKET FILE from fhrs $pfhr2 to $pfhr1 $pfhr3
    $EXECdng/smartprecip <<EOF > ${ppgm}precip${fhr}.out
$pfhr1 $pfhr2 $pfhr3 $pfhr4 
EOF
     export err=$?; #err_chk

     cp ${freq}precip.${fhr} ${freq}precip
     $utilexec/grbindex ${freq}precip ${freq}precipi
     cp ${freq}snow.${fhr} ${freq}snow
     $utilexec/grbindex ${freq}snow ${freq}snowi
      if [ $mk3p -eq 3 ]; then
#GFS already has 6 hour bucket at these forecasts so just extract precip and snow   
       $utilexec/wgrib -s gfs.t${cyc}z.smartinitin.guam.${fhr} | egrep "(:APCP:)" \
       | $utilexec/wgrib -i -grib -o 6precip gfs.t${cyc}z.smartinitin.guam.${fhr}
       $utilexec/grbindex 6precip 6precipi 
       $utilexec/wgrib -s gfs.t${cyc}z.smartinitin.guam.${fhr} | egrep "(:WEASD:)" \
       | $utilexec/wgrib -i -grib -o 6snow gfs.t${cyc}z.smartinitin.guam.${fhr}
       $utilexec/grbindex 6snow 6snowi
      fi
    fi #MKPCP>0
  done #MKPCP loop

#=================================================================
##  RUN PRODUCT GENERATOR
#=================================================================

  fi  #fhr -ne 0

  cp /com/date/t${cyc}z DATE
  if [ -s ${prdgfl}.${fhr} ];then    
    cp ${prdgfl}.${fhr} meso${rg}.NDFDf${fhr}  
    echo ${prdgfl}.${fhr} FOUND FOR FORECAST HOUR ${fhr}
  else
    echo $prdgfl NOT FOUND FOR FORECAST HOUR ${fhr}
    exit
  fi

  $utilexec/grbindex meso${rg}.NDFDf${fhr} meso${rg}.NDFDif${fhr}

#=================================================================
#   DECLARE INPUTS and RUN SMARTINIT 
#=================================================================

# CHANGE : for non-conus look in FIXdng for topo,land files
  cp $FIXdng/topo/${topofl} TOPONDFD
  cp $FIXdng/mask/${maskfl} LANDNDFD
  ln -sf TOPONDFD     fort.46
  ln -sf LANDNDFD     fort.48
  if [ $ext = grb ];then
    $utilexec/grbindex TOPONDFD TOPONDFDi
    $utilexec/grbindex LANDNDFD LANDNDFDi
    ln -sf TOPONDFDi  fort.47
    ln -sf LANDNDFDi  fort.49
  fi

  mksmart=1
  if [ $check3 -eq 0 -a $fhr -ne 00 ];then 
    if [ -s MAXMIN${fhr1}.tm00 ];then
      cp MAXMIN${fhr2}.tm00 MAXMIN2
      cp MAXMIN${fhr1}.tm00 MAXMIN1
    else
#     For 3 hourly input files, hourly maxmins not created
      ln -fs meso${rg}.NDFDf${fhr} MAXMIN2
      ln -fs meso${rg}.NDFDf${fhr} MAXMIN1
    fi
    $utilexec/grbindex MAXMIN1 MAXMIN1i
    $utilexec/grbindex MAXMIN2 MAXMIN2i
  fi
  freq=3;fmx=25   #fmx =  maxmin unit number for 1st maxmin file

  ln -sf "meso${rg}.NDFDf${fhr}"    fort.11
  ln -sf "meso${rg}.NDFDif${fhr}"   fort.12
  ln -sf "SREFPCP"                  fort.13
  ln -sf "SREFPCPi"                 fort.14

# At 12-hr times, input 12-hr max/min temps and 3 and 6-hr buckets
  case $fhr in 
    ${A6HR[0]}|${A6HR[1]}|${A6HR[2]}|${A6HR[3]}|${A6HR[4]}|${A6HR[5]}|${A6HR[6]}| \
    ${A6HR[7]}|${A6HR[8]})
    echo "********************************************************"
    echo RUN SMARTINIT for 12h valid 00 or 12Z fcst hours: $fhr

    cp $COMOUT/${mdl}.t${cyc}z.smart${outreg}${fhr3}.tm00 MAXMIN3
    cp $COMOUT/${mdl}.t${cyc}z.smart${outreg}${fhr6}.tm00 MAXMIN4
    cp $COMOUT/${mdl}.t${cyc}z.smart${outreg}${fhr9}.tm00 MAXMIN5
    $utilexec/grbindex MAXMIN3 MAXMIN3i
    $utilexec/grbindex MAXMIN4 MAXMIN4i
    $utilexec/grbindex MAXMIN5 MAXMIN5i

    ln -sf "3precip"    fort.15
    ln -sf "3precipi"   fort.16
    ln -sf "3snow"      fort.17
    ln -sf "3snowi"     fort.18
    ln -sf "6precip"    fort.19
    ln -sf "6precipi"   fort.20
    ln -sf "6snow"      fort.21
    ln -sf "6snowi"     fort.22
    ln -sf "12precip"   fort.23
    ln -sf "12precipi"  fort.24
    ln -sf "MAXMIN1"   fort.$fmx
    ln -sf "MAXMIN2"   fort.$((fmx+1))
    ln -sf "MAXMIN3"   fort.$((fmx+2))
    ln -sf "MAXMIN4"   fort.$((fmx+3))
    ln -sf "MAXMIN5"   fort.$((fmx+4))
    ln -sf "MAXMIN1i"  fort.$((fmx+5))
    ln -sf "MAXMIN2i"  fort.$((fmx+6))
    ln -sf "MAXMIN3i"  fort.$((fmx+7))
    ln -sf "MAXMIN4i"  fort.$((fmx+8))
    ln -sf "MAXMIN5i"  fort.$((fmx+9));;

    *)   # Not 00/12 UTC valid times
     if [ $check3 -eq 0 -a $fhr -ne $fhrstr ];then
#      READ PRECIP FROM SPECIAL FILES CREATED BY SMARTPRECIP
#      ON-CYC: All forecast hours divisible by 3 except for (3,15,27....), 
#      read  3-hr buckets max/min temp data for the previous 2 hours
#      OFF-CYC: Set input files to read 6 hr prcp from makeprecip files
       if [ $mk3p -eq 3 ];then
         echo "****************************************************************"
         case $cycon in
          1) echo RUN SMARTINIT for ON-CYC  hrs without 3 hr buckets : $fhr;;
          *) echo RUN SMARTINIT for OFF-CYC hrs without 6 hr buckets : $fhr;;
         esac
         ln -sf "3precip"   fort.15
         ln -sf "3precipi"  fort.16
         ln -sf "3snow"     fort.17
         ln -sf "3snowi"    fort.18
         ln -sf "6precip"   fort.19
         ln -sf "6precipi"  fort.20
         ln -sf "6snow"     fort.21
         ln -sf "6snowi"    fort.22
         ln -sf "MAXMIN2"   fort.23 
         ln -sf "MAXMIN1"   fort.24
         ln -sf "MAXMIN2i"  fort.25
         ln -sf "MAXMIN1i"  fort.26

       else           
#        READ PRECIP FROM INPUT MDL GRIB FILE 
#        ON-CYC:  Forecast hours 3,15,27,39....already  have 3-hr buckets,
#        OFF-CYC: 3 hour buckets available for all 3 hour forecast times
#        ALL-CYC: Input only  max/min temp data for the previous 2 hours

#         echo "****************************************************"
#         echo RUN SMARTINIT for hours with 3 hr buckets: $fhr
         ln -sf "MAXMIN2"   fort.15
         ln -sf "MAXMIN1"   fort.16
         ln -sf "MAXMIN2i"  fort.17
         ln -sf "MAXMIN1i"  fort.18
       fi  

     else   # fhr%3 -ne 0
#      For all "in-between" forecast hours (13,14,16....)
#      No special data needed
       echo "*****************************************************"
       echo RUN SMARTINIT for in-between hour: $fhr
       ln -fs " " fort.13
       ln -fs " " fort.14
       ln -fs " " fort.15
       ln -fs " " fort.16
       mksmart=0
#      Create downscaled 00 hour files 
       if [ $fhr -eq $fhrstr ];then mksmart=1;fi
     fi;;
  esac

#========================================================
# Run SMARTINIT
#========================================================
  hrlyfhr=12  # forecast hour to output hourly files to
  case $RUNTYP in
   conus|conusnest) RGIN=CS;;
      conusnest2p5) RGIN=CS2P;hrlyfhr=36;;
        ak_rtmages) RGIN=AKRT;;
           dgex_cs) RGIN=CS2P;;
           dgex_ak) RGIN=AK3;;
           guamnest) RGIN=GM;;
                 *) RGIN=`echo $rg |tr '[a-z]'  '[A-Z]' `;;
   esac

  export pgm=smartinit; # . prep_step
  ${EXECdng}/smartinit $cyc $fhr $ogrd $RGIN $inest $inhrfrq $fhrstr $core >smartinit.out${fhr}
  export err=$?; #err_chk

# Save hourly ak,hi,pr,conus2p5 nests and ak_rtmages(from nam parent) for RTMA 1st guess fields
#  if [ $fhr -le $hrlyfhr ];then
    case $RUNTYP in
     ak_rtmages) 
       cp MESO${RGIN}${fhr}.tm00  $COMOUT/${mdl}.t${cyc}z.smart${RUNTYP}${fhr}.tm00
       mksmart=0;;
     hawaiinest|priconest|conusnest2p5|aknest3|guamnest)
       cp MESO${RGIN}${fhr}.tm00  $COMOUT/${mdl}.t${cyc}z.smart${outreg}${fhr}.tm00;;
   esac
#  fi

  if [ $mksmart -eq 1 ];then
# Run NCO processing to convert output to grib2 and awips
   export RUNTYP
   export RGIN=$RGIN  # Region id (eg: CS, HI, PR,AK..)
   export outreg
   export cyc  
   export fhr=$fhr
   export ogrd 
   if [ $mdl = "hiresw" ];then
   ${USHdng}/dng_awp.sh $mdlgrd
   else
   ${USHdng}/dng_awp.sh $outreg
   fi

  fi
  echo
done  #fhr loop

exit
