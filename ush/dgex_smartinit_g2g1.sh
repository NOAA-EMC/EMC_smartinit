#!/bin/ksh 
#
# Author:        Geoff Manikin       Org: NP22         Date: 2007-08-06
#
# Script history log:
# 2007-08-06  Geoff Manikin
# 2012-07-24  Jeff McQueen  cleaned up redundant codes
#    Created precip threshold loop for creating sref prob precip files
# 2012-09-26  JTM : Modified for wcoss
#                   Combined on & off cycles options into one script 
# 2012-10-22  JTM : Combined addprecip and makeprecip codes
# 2012-10-26  JTM : Combined various nam region scripts
#                   smartinit: getgrib.f: fixed bug with reading sref prob file
# 2012-10-31  JTM : moved smartinit system to tide
# 2012-12-03  JTM : Unified for nam parent and nested region runs
# 2013-07-01  JTM : added extended conus (187) and guamnmmb, guamarw hrw options (199)
# 2013-07-03  JTM : Adding use of cfg file for grid settup
# 2013-08-27  JTM : Put in Vertical Structure
# 2013-11-20  JTM : Added option to downscale DGEX 3 hrly files beyond 84 hrs w/ 6 hr precip
# 2014-01-20  JTM : completed option to downscale hiresw guamnmmb, guamarw to 48 hours 
#======================================================================
#  Set Defaults fcst hours,cycle,model,region in smart_config_para called in parent job

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

# dgex_cs      :  SREF-GRID=212  DGEXGRID=dgex_conus.tCCz.bsmart  NDFD-GRD=197
# dgex_ak      :  SREF-GRID=216  DGEXGRID=dgex_alaska.tCCz.bsmart NDFD-GRD=198
#======================================================================

set -xa

# Check if this is a nest run
inest=`echo $RUNTYP|awk '{ print( index($0,"nest") )}' `

export rg=`echo $RUNTYP |cut -c1-2` 
tempvar=$(echo EXEC$mdl)
EXECmdl=$(eval echo \$$tempvar)
echo EXECmdl $EXECmdl

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
typeset -Z2 srefcyc gefscyc 
text=".tm00"

# Begin wgrib2

compress="c3 -set_bitmap 1"

case $RUNTYP in

  dgex_cs) natgrd=.bsmart; mdlgrd=conus; rg=dgx; outreg=conus; wgrib2def="lambert:265:25:25 238.446:1073:5079 20.192:689:5079";;
  dgex_ak) natgrd=.bsmart; mdlgrd=alaska; rg=dgx; outreg=ak; wgrib2def="nps:210:60 181.429:825:5953 40.53:553:5953";;

esac

# Define core (nmmb, arw, nems) needed for hiresw veg initialization
core=$rg
icore=`echo $RUNTYP|awk '{ print( index($0,"nmmb") )}' `
if [ $icore -eq 0 ];then 
  icore=`echo $RUNTYP|awk '{ print( index($0,"arw") )}' `
fi
if [ $icore -gt 0 ];then
  core=`echo $RUNTYP |cut -c $icore-`   
fi
if [ $mdl = "hiresw" ];then 
  inest=1
  text=;
fi

prdgfl=meso${rg}.NDFD  # output prdgen grid name (eg: mesocon.NDFD,mesoak...)
case $RUNTYP in
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
  00|12) set -A A6HR 12 24 36 48 60 72 84 96 108 120;;
  * )    set -A A6HR 18 30 42 54 66 78 90 102 114 126;;
esac
if [ $rg = dgx ];then
  inest=1  #set to read in 6hr precip for dgex files
  case $cyc in
    00|12) set -A A6HR 96 108 120 132 144 156 168 180 192;;
       * ) set -A A6HR 90 102 114 126 138 150 162 174 186;;
  esac
fi
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
echo GTYP $gtyp OGRD $ogrd
if [ $gtyp -ne $ogrd ];then
  case $gtyp in
#   kpds        1   2-9  10 11 12 13    14
      3) grid="255 $grid  0 64 25000 25000";;
      5) grid="255 $grid  0 64 25000 25000";;
      1) grid="255 $grid  0 64 2500 2500";;
  esac
fi

# Set NDFD output grid topo and land mask filenames
maskpre=${mdl}_smartmask${outreg}
topopre=${mdl}_smarttopo${outreg}
ext=grb
case $RUNTYP in dgex_cs|conus|conusnest) ext=dat;; esac
maskfl=${maskpre}.${ext}
topofl=${topopre}.${ext}

echo
echo "============================================================================"
echo BEGIN SMARTINIT PROCESSING FOR FFHR $ffhr  CYCLE $cyc
echo RUNTYP:  $RUNTYP mdlgrd: $mdlgrd  rg: $rg
echo INTERP GRID: $grid
echo OUTPUT GRID: $ogrd $outreg
# Begin wgrib2
echo INTERP GRID for wgrib2 : $wgrib2def
echo OUTPUT GRID: $outreg
# End wgrib2
echo "============================================================"
echo 

#  Set Defaults pcp hours and frequencies
let pcphr=ffhr+3
let pcphrl=ffhr+3
if [ $pcphrl -lt 10 ];then pcphrl="0"$pcphrl;fi
if [ $pcphrl -lt 100 ];then pcphrl="0"$pcphrl;fi
let pcphr12=pcphr-12
let pcphr6=pcphr-6
let pcphr3=pcphr-3

#======================================================================
#  CREATE SREF PROB. PRECIP FILES
#======================================================================

# fhr should be gt 0 since precip is not available at initial time
if [ $ffhr -gt ${fhrstr} ]; then

# Get the sref precip fields that we need
  if [ ! -s SREFPROB -o $rg = gm -o $rg = dgx ]; then
    waitsref=0
    waitend=1800
    while [ $waitsref -le $waitend ];do
      echo COMIN_GEFS $COMIN_GEFS
      if [ -s $COMIN_GEFS/${gefscyc}/sref.t${gefscyc}z.pgrb${sgrb}.prob_3hrly ];then 
        echo "SREF PROB FILE FOUND  CYC=" $gefscyc  GRID= $sgrb
        sleep 60
        cp $COMIN_GEFS/${gefscyc}/sref.t${gefscyc}z.pgrb${sgrb}.prob_3hrly SREFPROB
        break
      else
        sleep 60
        ((waitsref=waitsref+60))
         echo `date +%T` "WAITING For SREF Prob File"  CYC= $gefscyc  GRID= $sgrb $waitsref
        if [ $waitsref -gt $waitend ];then 
           echo GEFSCYC $gefscyc GRID $sgrib
           echo "SREF PROB FILE NOT AVAILABLE...RUN WITHOUT"
           break
        fi
      fi
    done
  else
    cp $COMIN_SREF/sref.t${srefcyc}z.pgrb${sgrb}.prob_3hrly SREFPROB
  fi
  $GRBINDEX SREFPROB SREFPROBI
 
  let IP=0
  if [ $ffhr -lt 6 ]; then pcphr6=;pcphr12=;fi
  if [ $ffhr -lt 12 ]; then pcphr12=;fi
  grbpre="2 0 0 0 0"

####set -x

  for PHR in $pcphr3 $pcphr6 $pcphr12;do 
#   prob of pcp > 0.01
    $WGRIB -PDS10 SREFPROB |grep "${grbpre} 64 64 0 0"|grep "0 1 $PHR $pcphr 4"|$WGRIB -i -grib -o dump SREFPROB
    let IP=IP+1
    mv dump srefpcp$IP

#   prob of pcp > 0.05
    $WGRIB -PDS10 SREFPROB |grep "${grbpre} 65 20 81 236"| grep "0 1 $PHR $pcphr 4"|$WGRIB -i -grib -o dump SREFPROB
    let IP=IP+1
    mv dump srefpcp$IP

#   prob of pcp > 0.10
    $WGRIB -PDS10 SREFPROB |grep "${grbpre} 65 40 163 215"| grep "0 1 $PHR $pcphr 4"|$WGRIB -i -grib -o dump SREFPROB
    let IP=IP+1
    mv dump srefpcp$IP

#   prob of pcp > 0.25
    $WGRIB -PDS10 SREFPROB |grep "${grbpre} 65 101 153 154"| grep "0 1 $PHR $pcphr 4"|$WGRIB -i -grib -o dump SREFPROB
    let IP=IP+1
    mv dump srefpcp$IP

#   prob of pcp > 0.50
    $WGRIB -PDS10 SREFPROB |grep "${grbpre} 65 203 51 51"| grep "0 1 $PHR $pcphr 4"|$WGRIB -i -grib -o dump SREFPROB
    let IP=IP+1
    mv dump srefpcp$IP
  done
#set +x

  cat srefpcp1 srefpcp2 srefpcp3 srefpcp4 srefpcp5 > srefallpcp
  if [ $ffhr -ge 6 ]; then
    cat srefpcp6 srefpcp7 srefpcp8 srefpcp9 srefpcp10 >> srefallpcp
  fi
  if [ $ffhr -ge 12 ]; then
    cat srefpcp11 srefpcp12 srefpcp13 srefpcp14 srefpcp15 >> srefallpcp
  fi

  $COPYGB -g "$grid" -x srefallpcp srefpcp${rg}_${SREF_PDY}${srefcyc}f${pcphrl}
  $GRBINDEX srefpcp${rg}_${SREF_PDY}${srefcyc}f${pcphrl} srefpcp${rg}i_${SREF_PDY}${srefcyc}f${pcphrl}

fi #fhr -ge 0

let ffhr1=ffhr-1
let ffhr2=ffhr-2
hours="${ffhr}"
if [ $ffhr -ge 3 ];then hours="${ffhr2} ${ffhr1} ${ffhr}";fi
if [ $rg = dgx ];then hours="${ffhr}";fi   #DGEX only has output every 3 hrs

#===========================================================
#  CREATE Accum precip buckets if necessary 
#===========================================================
for fhr in $hours; do
  rm -f *out${fhr}
  mk3p=0;mk6p=0;mk12p=0
  let check=fhr%3
  let check6=fhr%6
  let fhr1=fhr-1
  let fhr2=fhr-2
  let fhr3=fhr-3
  let fhr6=fhr-6
  let fhr9=fhr-9
  if [ $fhr -gt 00 ];then 
   if [ $fhr -lt 10 -a $check -ne 0 ];then fhr="0"${fhr};fi
   if [ $fhr1 -lt 10 ];then fhr1="0"${fhr1};fi
   if [ $fhr2 -lt 10 ];then fhr2="0"${fhr2};fi
   if [ $fhr3 -lt 10 ];then fhr3="0"${fhr3};fi
   if [ $fhr6 -lt 10 ];then fhr6="0"${fhr6};fi
   if [ $fhr9 -lt 10 ];then fhr9="0"${fhr9};fi
  fi
  echo FHR FHR1 FHR2 FHR3 FHR6 FHR9  $fhr $fhr1 $fhr2 $fhr3 $fhr6 $fhr9

# Check that 00 hr analysis is from NDAS or GDAS
    case $natgrd in 
      bgrd3d) 
#     Check that  00 hr analysis is from NDAS or GDAS (08/2013)
        if [ $fhr -eq 00 -a $GUESS = GDAS ];then
          echo;echo "WARNING  GUESS = " $GUESS INDICATES $mdl COLD START
          echo USING PREVIOUS $pcdate $ ${cyc}Z CYCLE $mdl $pcfhr FORECAST;echo
          mdlin=${COM_IN}/${mdl}.${pcdate}/${mdl}.t${pcyc}z.${natgrd}
          ln -fs ${mdlin}${pcfhr}.tm00 fort.11
          ln -fs WRFPRS${fhr}.tm00 fort.51
#         echo ${PDY}${cyc} | $OVERDATEGRIB
          echo ${PDY}${cyc} | ${utilexec}/overdate.grib
        else
          echo;echo $mdl GUESS= $GUESS
          mdlin=$COMIN/${mdl}.t${cyc}z.${natgrd}
          cp ${mdlin}${fhr}${text} WRFPRS${fhr}.tm00
        fi
#       Reduce the input model file size for prdgen on wcoss 32 bit limited machines
        $WGRIB -s WRFPRS${fhr}.tm00 | \
        grep -f ${PARMdng}/${mdl}_smartinit.parmlist | \
        $WGRIB -i -grib -o temp WRFPRS${fhr}.tm00 > wgrib.out
        mv temp WRFPRS${fhr}.tm00;;

      wrfprs)  
        mdlin=$COMIN/${mdlgrd}.t${cyc}z.${natgrd}
        cp ${mdlin}${fhr}${text} WRFPRS${fhr}.tm00;;
           *) 
        if [ $rg = dgx ];then 
          mdlin=$COMIN/${mdl}_${mdlgrd}.t${cyc}z${natgrd}
          cp ${mdlin}${fhr}.tm00 WRFPRS${fhr}.tm00
        else
          mdlin=$COMIN/${mdl}.t${cyc}z.${mdlgrd}${natgrd}
          cp ${mdlin}${fhr}.tm00 WRFPRS${fhr}.tm00
        fi;;
    esac

# Begin wgrib2 - comment out
# $GRBINDEX WRFPRS${fhr}.tm00 WRFPRS${fhr}i.tm00
# End wgrib2

  inhrfrq=1

  if [ $fhr -gt ${fhrstr} ];then
#   Check if hourly or 3 hourly input files needed to determine maxmin read frequency
    if [ ${rg} = dgx ];then inhrfrq=3;fi

# sminit_mkprcp.sh ######################################
#-------------------------------------------------------------
#   OFF-CYC & Nests: Create 6/12 hour buckets, 3 hr buckets available
#   ON-CYC :
#     3hr precip available at only 3,15, 27,39... forcast fhours
#     other hours, create 3 hr precip
#     6hr precip: Create only at 00/12 UTC valid times 
#           eg: fhr=12,24,36
#     Create 12 hour precip at 00/12 UTC valid times
#-------------------------------------------------------------
    mk3p=0
    if [ $check6 -eq 0 -a $rg != dgx ];then 
      mk6p=6
      ppgm=add
    fi
#   hr3bkt flag determines when to run smartprecip to create 3 hr buckets
    let hr3bkt=$((fhr-3))%12

#-------------------------------------------------------------
#   ON-CYC: The 3-hr fhrs (3,15,27,39....) --> Already have 3-hr buckets
#   For in-between fhrs (22,23,25) --> Create 3-hr buckets
#   since we only gather max/min data at those hours to compute 12 hr max/mins
#   DGEX has 6 hr precip at 6hr times and 3 hr precip at other times
#-------------------------------------------------------------
    if [ $cycon -eq 1 -a $inest -eq 0 ];then
      if [ $hr3bkt -ne 0 -a $check -eq 0 ];then
        mk3p=3
        ppgm=make
      fi
    else
      hr3bkt=0
      if [ $rg = dgx ];then 
       if [ $check6 -eq 0 ];then
         mk3p=3;ppgm=make;hr3bkt=3   # create dgex 3 hr precip at 6 hr time intervals
       else
         if [ `expr $fhr - $fhrstr` -gt 3 ];then mk6p=6;ppgm=make;fi
       fi
      fi
    fi #cycon check
  fi  #fhr -ne 0

#-------------------------------------------------------------
# ON-CYCLE:  At 12-hr times:  Need 6 hour buckets as well
# Except for 6 hr times (18,30,42...) : Already have 6 hour buckets
# In addition, For 00/12 UTC valid times: Need to make 12 hour accumulations
#-------------------------------------------------------------
  case $fhr in 
    ${A6HR[0]}|${A6HR[1]}|${A6HR[2]}|${A6HR[3]}|${A6HR[4]}|${A6HR[5]}|${A6HR[6]}| \
    ${A6HR[7]}|${A6HR[8]} )
    if [ $cycon -eq 1 -a inest -eq 0 ];then
      mk6p=6
      ppgm=make
    else
#     off-cycles and  Nests have 3 hr buckets but need 6,12 hour precip
      mk6p=6
      mk12p=12
      ppgm=add
      if [ $rg = dgx ];then mk6p=0;fi   #dgex already has 6 hr precip in std wrfprs file
    fi;;
  esac 

  echo MKPCP Flags: MK3P $mk3p   MK6P $mk6p   MK12P $mk12p
  for MKPCP in $mk3p $mk6p $mk12p;do
    if [ $MKPCP -ne 0 ];then
      echo ====================================================================
      echo BEGIN Making $MKPCP hr PRECIP Buckets for $fhr Hour $ppgm freq $freq
      echo ====================================================================
      pfhr3=-99;pfhr4=-99
# Begin GRIB2
        cp WRFPRS${fhr}.tm00 WRFPRS${fhr}.tm00.grb2
        $CNVGRIB -g21 WRFPRS${fhr}.tm00.grb2 WRFPRS${fhr}.tm00.grb
        $GRBINDEX WRFPRS${fhr}.tm00.grb WRFPRS${fhr}i.tm00.grb
# End GRIB2
#     ln -sf "WRFPRS${fhr}.tm00"     fort.15   # ln current forecast hour file for 3,6hr precip
#     ln -sf "WRFPRS${fhr}i.tm00"    fort.16
      ln -sf "WRFPRS${fhr}.tm00.grb"     fort.15   # ln current forecast hour file for 3,6hr precip
      ln -sf "WRFPRS${fhr}i.tm00.grb"    fort.16
      case $MKPCP in
        $mk3p )
          FHRFRQ=$fhr3;freq=3
          pfhr1=$fhr;pfhr2=$fhr3
          if [ $rg = dgx ];then pfhr3=$fhr6;fi;;  

        $mk6p )
          FHRFRQ=$fhr6;freq=6
          pfhr1=$fhr;pfhr2=$fhr6    # fhr-fhr6
          if [ $ppgm = add ];then 
            FHRFRQ=$fhr3
            pfhr1=$fhr3;pfhr2=$fhr  # fhr3+fhr
          fi
          if [ $rg = dgx ];then
            ppgm=addsub
            pfhr1=$fhr6;pfhr2=$fhr3;pfhr3=$fhr         # fhr + (fhr3-fhr6)
# Begin wgrib2
#           cp ${mdlin}${fhr3}${text} WRFPRS${fhr3}.tm00
            cp ${mdlin}${fhr3}${text} WRFPRS${fhr3}.tm00.grb2
            $CNVGRIB -g21 WRFPRS${fhr3}.tm00.grb2 WRFPRS${fhr3}.tm00
            $GRBINDEX WRFPRS${fhr3}.tm00 WRFPRS${fhr3}i.tm00
            cp WRFPRS${fhr}.tm00 WRFPRS${fhr}.tm00.grb2
            $CNVGRIB -g21 WRFPRS${fhr}.tm00.grb2 WRFPRS${fhr}.tm00.grb
            $GRBINDEX WRFPRS${fhr}.tm00.grb WRFPRS${fhr}i.tm00.grb
# End wgrib2
            ln -sf "WRFPRS${fhr3}.tm00"     fort.15    # Will contain 6 hr precip
            ln -sf "WRFPRS${fhr3}i.tm00"    fort.16
#           ln -sf "WRFPRS${fhr}.tm00"     fort.17
#           ln -sf "WRFPRS${fhr}i.tm00"    fort.18
            ln -sf "WRFPRS${fhr}.tm00.grb"     fort.17
            ln -sf "WRFPRS${fhr}i.tm00.grb"    fort.18
          fi;;

        $mk12p )
          FHRFRQ=$fhr9;freq=12
          pfhr1=$fhr9;pfhr2=$fhr6;pfhr3=$fhr3;pfhr4=$fhr
# Begin wgrib2
          cp ${mdlin}${fhr6}${text} WRFPRS${fhr6}.tm00.grb2
          $CNVGRIB -g21 WRFPRS${fhr6}.tm00.grb2 WRFPRS${fhr6}.tm00
          $GRBINDEX WRFPRS${fhr6}.tm00 WRFPRS${fhr3}i.tm00
# End wgrib2
          ln -sf "WRFPRS${fhr6}.tm00"      fort.15    
          ln -sf "WRFPRS${fhr6}i.tm00"     fort.16

#         DGEX 12 hr precip = Precip@fh6 + Precip@fhr   
          if [ $rg = dgx ];then 
            ppgm=add
            FHRFRQ=$fhr6
            pfhr1=$fhr6;pfhr2=$fhr;pfhr3=$fhr;pfhr4=$fhr
#           ln -sf "WRFPRS${fhr}.tm00"      fort.15    
#           ln -sf "WRFPRS${fhr}i.tm00"     fort.16
            ln -sf "WRFPRS${fhr}.tm00.grb"      fort.15    
            ln -sf "WRFPRS${fhr}i.tm00.grb"     fort.16
          fi;;  
      esac
# Begin wgrib2
#     cp ${mdlin}${FHRFRQ}${text} WRFPRS${FHRFRQ}.tm00
      cp ${mdlin}${FHRFRQ}${text} WRFPRS${FHRFRQ}.tm00.grb2
      $CNVGRIB -g21 WRFPRS${FHRFRQ}.tm00.grb2 WRFPRS${FHRFRQ}.tm00
# End wgrib2
      case $natgrd in bgrd3d) 
        $WGRIB -s WRFPRS${FHRFRQ}.tm00 |grep -f ${PARMdng}/${mdl}_smartinit.parmlist | \
        $WGRIB -i -grib -o temp WRFPRS${FHRFRQ}.tm00 > wgrib.out
        mv temp WRFPRS${FHRFRQ}.tm00;;
      esac
      $GRBINDEX WRFPRS${FHRFRQ}.tm00 WRFPRS${FHRFRQ}i.tm00

      export pgm=${mdl}_smartprecip; . prep_step
      ln -sf "WRFPRS${FHRFRQ}.tm00"  fort.13  
      ln -sf "WRFPRS${FHRFRQ}i.tm00" fort.14
      ln -sf "${freq}precip.${fhr}"  fort.50
      ln -sf "${freq}cprecip.${fhr}" fort.51
      ln -sf "${freq}snow.${fhr}"    fort.52

      if [ $MKPCP -eq $mk12p -a $pfhr4 -gt 0 ];then
# Begin wgrib2
#       cp ${mdlin}${fhr3}${text} WRFPRS${fhr3}.tm00
        cp ${mdlin}${fhr3}${text} WRFPRS${fhr3}.tm00.grb2
        $CNVGRIB -g21 WRFPRS${fhr3}.tm00.grb2 WRFPRS${fhr3}.tm00
# End wgrib2
        case $natgrd in bgrd3d) 
          $WGRIB -s WRFPRS${fhr3}.tm00 |grep -f ${PARMdng}/nam_smartinit.parmlist | \
          $WGRIB -i -grib -o temp WRFPRS${fhr3}.tm00 > wgrib.out
          mv temp WRFPRS${fhr3}.tm00;;
        esac
        $GRBINDEX WRFPRS${fhr3}.tm00 WRFPRS${fhr3}i.tm00

# Begin wgrib2
#       cp ${mdlin}${fhr6}${text} WRFPRS${fhr6}.tm00
        cp ${mdlin}${fhr6}${text} WRFPRS${fhr6}.tm00.grb2
        $CNVGRIB -g21 WRFPRS${fhr6}.tm00.grb2 WRFPRS${fhr6}.tm00
# End wgrib2
        case $natgrd in bgrd3d) 
          $WGRIB -s WRFPRS${fhr6}.tm00 |grep -f ${PARMdng}/nam_smartinit.parmlist | \
          $WGRIB -i -grib -o temp WRFPRS${fhr6}.tm00 > wgrib.out
          mv temp WRFPRS${fhr6}.tm00;;
        esac
        $GRBINDEX WRFPRS${fhr6}.tm00 WRFPRS${fhr6}i.tm00

# link 6hr?
        ln -sf "WRFPRS${fhr3}.tm00"      fort.17
        ln -sf "WRFPRS${fhr3}i.tm00"     fort.18
#       ln -sf "WRFPRS${fhr}.tm00"       fort.19
#       ln -sf "WRFPRS${fhr}i.tm00"      fort.20
        ln -sf "WRFPRS${fhr}.tm00.grb"       fort.19
        ln -sf "WRFPRS${fhr}i.tm00.grb"      fort.20
      fi  # mk12p

#===============================================================
# smartprecip : Create Precip Buckets for smartinit 
#  if pfhr1 > pfhr2: create 3hr precip=prcp:fhr - prcp-3 -->  All 3hr buckets
#  This option also used for dgex to create 3hr precip at 6 hr times, check6=0
#  if pfhr1 < pfhr2: create 6hr precip=prcp-3 +prcp:fhr  -->  All 3 hr buckets
#     and pfhr3>0  : Create 3hr precip=prcp:fhr + (prcp:fhr-3 - prcp:fhr-6)
#     Note: prcp:fhr-6, assumed to be 6 hr bucket
#  if pfhr4 > 0    : create 12h precip=prcp-9 + prcp-6 + prcp-3 +prcp:fhr, All 3hr buckets
#===============================================================
      echo MAKE $freq HR PRECIP BUCKET FILE from fhrs $pfhr1 to $pfhr2 $pfhr3 $pfhr4
      $EXECdng/smartprecip <<EOF > ${ppgm}precip${freq}.out${fhr}
$pfhr1 $pfhr2 $pfhr3 $pfhr4 
EOF
      export err=$?;  err_chk

#     Interp precip to smartinit GRID
      cpgbgrd=$grid
      if [ $inest -gt 0 ];then cpgbgrd=$ogrd;fi
      if [ $RUNTYP = aknest3 ];then cpgbgrd=$grid;fi
      $COPYGB -g "$cpgbgrd" -i3 -x ${freq}precip.${fhr} ${freq}precip
      $GRBINDEX ${freq}precip ${freq}precipi
      $COPYGB -g "$cpgbgrd" -i3 -x ${freq}snow.${fhr} ${freq}snow
      $GRBINDEX ${freq}snow ${freq}snowi
    fi #MKPCP>0
  done #MKPCP loop

#=================================================================
#  RUN PRODUCT GENERATOR
#=================================================================
# Begin wgrib2
interp="-new_grid_interpolation bilinear"
# use either bilinear or nearest neighbor interpolation, dependent on domain
# dgex_cs uses bilinear -> going from 12 km to 5 km;
# dgex_ak uses bilinear -> going from 12 km to 6 km;

if [ -e inputs.grb2_1 ]
then
rm inputs.grb2_1 inputs.grb2_2 inputs.grb2_3 inputs.grb2_4 inputs.grb2_5 inputs.grb2_6 inputs.grb2_7 inputs.grb2_8 inputs.grb2_9 inputs.grb2_10 inputsb.grb2_1 inputsb.grb2_2 inputsn.grb2
fi

ngrd=$natgrd
if [ $natgrd = ".bsmart" ];then
  ngrd=bsmart
fi

cp -p $PARMdng/nam_smartinit_${ngrd}_grb2.parmlist_1 inventory.txt1
cp -p $PARMdng/nam_smartinit_${ngrd}_grb2.parmlist_2 inventory.txt2
cp -p $PARMdng/nam_smartinit_${ngrd}_grb2.parmlist_3 inventory.txt3
cp -p $PARMdng/nam_smartinit_${ngrd}_grb2.parmlist_4 inventory.txt4
cp -p $PARMdng/nam_smartinit_${ngrd}_grb2.parmlist_5 inventory.txt5
cp -p $PARMdng/nam_smartinit_${ngrd}_grb2.parmlist_6 inventory.txt6
cp -p $PARMdng/nam_smartinit_${ngrd}_grb2.parmlist_7 inventory.txt7
cp -p $PARMdng/nam_smartinit_${ngrd}_grb2.parmlist_8 inventory.txt8
cp -p $PARMdng/nam_smartinit_${ngrd}_grb2.parmlist_9 inventory.txt9
cp -p $PARMdng/nam_smartinit_${ngrd}_grb2.parmlist_10 inventory.txt10
cp -p $PARMdng/nam_smartinit_grb2_budget.parmlist_1 inventoryb.txt1
cp -p $PARMdng/nam_smartinit_grb2_budget.parmlist_2 inventoryb.txt2
cp -p $PARMdng/nam_smartinit_grb2_nn.parmlist inventoryn.txt

$WGRIB2 WRFPRS${fhr}.tm00 | grep -F -f inventory.txt1 | $WGRIB2 -i -grib inputs.grb2_1 WRFPRS${fhr}.tm00
$WGRIB2 WRFPRS${fhr}.tm00 | grep -F -f inventory.txt2 | $WGRIB2 -i -grib inputs.grb2_2 WRFPRS${fhr}.tm00
$WGRIB2 WRFPRS${fhr}.tm00 | grep -F -f inventory.txt3 | $WGRIB2 -i -grib inputs.grb2_3 WRFPRS${fhr}.tm00
$WGRIB2 WRFPRS${fhr}.tm00 | grep -F -f inventory.txt4 | $WGRIB2 -i -grib inputs.grb2_4 WRFPRS${fhr}.tm00
$WGRIB2 WRFPRS${fhr}.tm00 | grep -F -f inventory.txt5 | $WGRIB2 -i -grib inputs.grb2_5 WRFPRS${fhr}.tm00
$WGRIB2 WRFPRS${fhr}.tm00 | grep -F -f inventory.txt6 | $WGRIB2 -i -grib inputs.grb2_6 WRFPRS${fhr}.tm00
$WGRIB2 WRFPRS${fhr}.tm00 | grep -F -f inventory.txt7 | $WGRIB2 -i -grib inputs.grb2_7 WRFPRS${fhr}.tm00
$WGRIB2 WRFPRS${fhr}.tm00 | grep -F -f inventory.txt8 | $WGRIB2 -i -grib inputs.grb2_8 WRFPRS${fhr}.tm00
$WGRIB2 WRFPRS${fhr}.tm00 | grep -F -f inventory.txt9 | $WGRIB2 -i -grib inputs.grb2_9 WRFPRS${fhr}.tm00
$WGRIB2 WRFPRS${fhr}.tm00 | grep -F -f inventory.txt10 | $WGRIB2 -i -grib inputs.grb2_10 WRFPRS${fhr}.tm00
$WGRIB2 WRFPRS${fhr}.tm00 | grep -F -f inventoryb.txt1 | $WGRIB2 -i -grib inputsb.grb2_1 WRFPRS${fhr}.tm00
$WGRIB2 WRFPRS${fhr}.tm00 | grep -F -f inventoryb.txt2 | $WGRIB2 -i -grib inputsb.grb2_2 WRFPRS${fhr}.tm00
$WGRIB2 WRFPRS${fhr}.tm00 | grep -F -f inventoryn.txt | $WGRIB2 -i -grib inputsn.grb2 WRFPRS${fhr}.tm00

rm wgrib2.poe

if [ -e model.ndfd_1 ]
then
rm  model.ndfd_1  model.ndfd_2  model.ndfd_3  model.ndfd_4  model.ndfd_5 model.ndfd_6
rm  model.ndfd_7 model.ndfd_8 model.ndfd_9 model.ndfd_10 model.ndfd_b1 model.ndfd_b2 model.ndfd_n
fi
echo "#! /bin/ksh" > a.poe
echo "$WGRIB2 inputs.grb2_1 -set_grib_type ${compress} -new_grid_winds grid ${interp} -new_grid ${wgrib2def} model.ndfd_1" >> a.poe
echo "#! /bin/ksh" > b.poe
echo "$WGRIB2 inputs.grb2_2 -set_grib_type ${compress} -new_grid_winds grid ${interp} -new_grid ${wgrib2def} model.ndfd_2" >> b.poe
echo "#! /bin/ksh" > c.poe
echo "$WGRIB2 inputs.grb2_3 -set_grib_type ${compress} -new_grid_winds grid ${interp} -new_grid ${wgrib2def} model.ndfd_3" >> c.poe
echo "#! /bin/ksh" > d.poe
echo "$WGRIB2 inputs.grb2_4 -set_grib_type ${compress} -new_grid_winds grid ${interp} -new_grid ${wgrib2def} model.ndfd_4" >> d.poe
echo "#! /bin/ksh" > e.poe
echo "$WGRIB2 inputs.grb2_5 -set_grib_type ${compress} -new_grid_winds grid ${interp} -new_grid ${wgrib2def} model.ndfd_5" >> e.poe
echo "#! /bin/ksh" > f.poe
echo "$WGRIB2 inputs.grb2_6 -set_grib_type ${compress} -new_grid_winds grid ${interp} -new_grid ${wgrib2def} model.ndfd_6" >> f.poe
echo "#! /bin/ksh" > g.poe
echo "$WGRIB2 inputs.grb2_7 -set_grib_type ${compress} -new_grid_winds grid ${interp} -new_grid ${wgrib2def} model.ndfd_7" >> g.poe
echo "#! /bin/ksh" > h.poe
echo "$WGRIB2 inputs.grb2_8 -set_grib_type ${compress} -new_grid_winds grid ${interp} -new_grid ${wgrib2def} model.ndfd_8" >> h.poe
echo "#! /bin/ksh" > i.poe
echo "$WGRIB2 inputs.grb2_9 -set_grib_type ${compress} -new_grid_winds grid ${interp} -new_grid ${wgrib2def} model.ndfd_9" >> i.poe
echo "#! /bin/ksh" > j.poe
echo "$WGRIB2 inputs.grb2_10 -set_grib_type ${compress} -new_grid_winds grid ${interp} -new_grid ${wgrib2def} model.ndfd_10" >> j.poe

# always use budget interpolation for precip and snow 

interp="-new_grid_interpolation budget"
echo "#! /bin/ksh" > k.poe
echo "$WGRIB2 inputsb.grb2_1 -set_grib_type ${compress} -new_grid_winds grid ${interp} -new_grid ${wgrib2def} model.ndfd_b1" >> k.poe
echo "#! /bin/ksh" > l.poe
echo "$WGRIB2 inputsb.grb2_2 -set_grib_type ${compress} -new_grid_winds grid ${interp} -new_grid ${wgrib2def} model.ndfd_b2" >> l.poe

# always use nearest neighbor interpolation for these fields

interp="-new_grid_interpolation neighbor"
#cp -p $PARMdng/nam_smartinit_grb2_nn.parmlist inventoryn.txt
#$WGRIB2 WRFPRS${fhr}.tm00 | grep -F -f inventoryn.txt | $WGRIB2 -i -grib inputsn.grb2 WRFPRS${fhr}.tm00
$WGRIB2 inputsn.grb2 -set_grib_type ${compress} -new_grid_winds grid ${interp} -new_grid ${wgrib2def} model.ndfd_n

chmod 775 a.poe
chmod 775 b.poe
chmod 775 c.poe
chmod 775 d.poe
chmod 775 e.poe
chmod 775 f.poe
chmod 775 g.poe
chmod 775 h.poe
chmod 775 i.poe
chmod 775 j.poe
chmod 775 k.poe
chmod 775 l.poe

echo "a.poe" > wgrib2.poe
echo "b.poe" >> wgrib2.poe
echo "c.poe" >> wgrib2.poe
echo "d.poe" >> wgrib2.poe
echo "e.poe" >> wgrib2.poe
echo "f.poe" >> wgrib2.poe
echo "g.poe" >> wgrib2.poe
echo "h.poe" >> wgrib2.poe
echo "i.poe" >> wgrib2.poe
echo "j.poe" >> wgrib2.poe
echo "k.poe" >> wgrib2.poe
echo "l.poe" >> wgrib2.poe

chmod 775 wgrib2.poe
export MP_PGMMODEL=mpmd
export MP_CMDFILE=wgrib2.poe
time mpirun.lsf
export err=$?;  err_chk

cat model.ndfd_1 model.ndfd_2 model.ndfd_3 model.ndfd_4 model.ndfd_5 model.ndfd_6 \
    model.ndfd_7 model.ndfd_8 model.ndfd_9 model.ndfd_10 model.ndfd_b1 model.ndfd_b2 model.ndfd_n > ${prdgfl}.grb2

# End parallel wgrib2
# convert to grib1

#cnvgrib -g21 ${prdgfl}.grb2 ${prdgfl}
$CNVGRIB -g21 ${prdgfl}.grb2 ${prdgfl}

# End wgrib2

# $GRBINDEX WRFPRS${fhr}.tm00 WRFPRS${fhr}i.tm00
# echo creating $prdgfl file for fhr $fhr
# cat >input${fhr}.prd <<EOF5
#WRFPRS${fhr}.tm00
#EOF5

# cp/ln PRDGEN master ctl and weight files
# if [ $inest -eq 0 ];then
#   cp -p $PARMdng/${mdl}_master${outreg}.ctl master${fhr}.ctl
#   ln -sf $FIXdng/wgt/${mdl}_wgt_${ogrd}     fort.21
# else
#   To interp nests to 5 km, just use same parent nam master files 
#   To interp ak/cs nests to ak3/cs2p5, use special smartmaster ctl files
#   case $rg in
#        ak|hi|pr) cp -p $PARMdng/${mdl}_master${rg}.ctl master${fhr}.ctl;;
#      con|ak3|gm) cp -p $PARMdng/${mdl}_smartmaster${RUNTYP}.ctl master${fhr}.ctl;;
#             dgx) cp -p $PARMdng/${mdl}_master${outreg}.ctl master${fhr}.ctl;;
#   esac
#   ln -sf $FIXdng/wgt/${mdl}_wgt_${ogrd}_${mdlgrd} fort.21
# fi

# export pgm=${mdl}_prdgen; . prep_step
# ln -sf master${fhr}.ctl            fort.10
# ln -sf input${fhr}.prd             fort.621   #WCOSS CHANGE

# POINT TO NETwork prdgen (/nwprod/exec) 
# ${EXECmdl}/${mdl}_prdgen < input${fhr}.prd > prdgen.out${fhr}
# export err=$?;  err_chk

# cp /com/date/t${cyc}z DATE
  cp ${COMROOT}/date/t${cyc}z DATE
  if [ -s $prdgfl ];then  
    mv ${prdgfl} meso${rg}.NDFDf${fhr}  
    echo $prdgfl FOUND FOR FORECAST HOUR ${fhr}
  elif [ -s ${prdgfl}${fhr} ];then    # check for hawaii ???
    mv ${prdgfl}${fhr} meso${rg}.NDFDf${fhr}  
    echo $prdgfl${fhr} FOUND FOR FORECAST HOUR ${fhr}
  else
    echo $prdgfl NOT FOUND FOR FORECAST HOUR ${fhr}
    exit
  fi
  $GRBINDEX meso${rg}.NDFDf${fhr} meso${rg}.NDFDif${fhr}

#=================================================================
#   DECLARE INPUTS and RUN SMARTINIT 
#=================================================================

  cp $FIXdng/topo/${topofl} TOPONDFD
  cp $FIXdng/mask/${maskfl} LANDNDFD
  ln -sf TOPONDFD     fort.46
  ln -sf LANDNDFD     fort.48
  if [ $ext = grb ];then
    $GRBINDEX TOPONDFD TOPONDFDi
    $GRBINDEX LANDNDFD LANDNDFDi
    ln -sf TOPONDFDi  fort.47
    ln -sf LANDNDFDi  fort.49
  fi

  mksmart=1
  if [ $check -eq 0 -a $fhr -ne $fhrstr ];then 
    cp srefpcp${rg}_${SREF_PDY}${srefcyc}f${pcphrl} SREFPCP
    cp srefpcp${rg}i_${SREF_PDY}${srefcyc}f${pcphrl} SREFPCPi
    if [ -s MAXMIN${fhr1}.tm00 ];then
      cp MAXMIN${fhr2}.tm00 MAXMIN2
      cp MAXMIN${fhr1}.tm00 MAXMIN1
    else
#     For 3 hourly input files, hourly maxmins not created
      ln -fs meso${rg}.NDFDf${fhr} MAXMIN2
      ln -fs meso${rg}.NDFDf${fhr} MAXMIN1
    fi
    $GRBINDEX MAXMIN1 MAXMIN1i
    $GRBINDEX MAXMIN2 MAXMIN2i
  fi
  freq=6;fmx=21   #fmx =  maxmin unit number for 1st maxmin file
  if [ $rg = dgx -a $check6 -eq 0 ];then freq=3;fi  #dgx created 3 hr precip file at 6 hr times
  if [ $cycon -eq 1 ];then 
    if [ $inest -eq 0 ];then 
      freq=3;fmx=23
    fi
  else
    fmx=19   
  fi

  ln -sf "meso${rg}.NDFDf${fhr}"    fort.11
  ln -sf "meso${rg}.NDFDif${fhr}"   fort.12
  ln -sf "SREFPCP"                  fort.13
  ln -sf "SREFPCPi"                 fort.14
  ln -sf "${freq}precip"            fort.15
  ln -sf "${freq}precipi"           fort.16

# At 12-hr times, input 12-hr max/min temps and 3 and 6-hr buckets
  case $fhr in 
    ${A6HR[0]}|${A6HR[1]}|${A6HR[2]}|${A6HR[3]}|${A6HR[4]}|${A6HR[5]}|${A6HR[6]}| \
    ${A6HR[7]}|${A6HR[8]})
    echo "********************************************************"
    echo RUN SMARTINIT for 12h valid 00 or 12Z fcst hours: $fhr

    if [ $cycon -eq 0 ];then fmx=21;fi
    cp $COMOUT/${mdl}.t${cyc}z.smart${outreg}${fhr3}.tm00 MAXMIN3
    cp $COMOUT/${mdl}.t${cyc}z.smart${outreg}${fhr6}.tm00 MAXMIN4
    cp $COMOUT/${mdl}.t${cyc}z.smart${outreg}${fhr9}.tm00 MAXMIN5
    $GRBINDEX MAXMIN3 MAXMIN3i
    $GRBINDEX MAXMIN4 MAXMIN4i
    $GRBINDEX MAXMIN5 MAXMIN5i

    if [ $cycon -eq 1 -a inest -eq 0 ];then
#     READ 3/6 hr precip from special files created by makeprecip
      ln -sf "6precip"   fort.17
      ln -sf "6precipi"  fort.18
      ln -sf "3snow"     fort.19
      ln -sf "3snowi"    fort.20
      ln -sf "6snow"     fort.21
      ln -sf "6snowi"    fort.22
    else     
#     READ 6/12 hr precip from special files created by makeprecip
      ln -sf "${freq}snow"      fort.17       # For DGEX, read 3hr special precip file at check6=0
      ln -sf "${freq}snowi"     fort.18
      ln -sf "12precip"   fort.19
      ln -sf "12precipi"  fort.20
    fi   
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
     if [ $check -eq 0 -a $fhr -ne $fhrstr ];then
#      READ PRECIP FROM SPECIAL FILES CREATED BY SMARTPRECIP
#      ON-CYC: All forecast hours divisible by 3 except for (3,15,27....), 
#      read  3-hr buckets max/min temp data for the previous 2 hours
#      OFF-CYC: Set input files to read 6 hr prcp from makeprecip files
       if [ $hr3bkt -ne 0 -o $mk6p -ne 0 ];then
         echo "****************************************************************"
         case $cycon in
           1) echo RUN SMARTINIT for ON-CYC  hrs without 3 hr buckets : $fhr;;
           *) echo RUN SMARTINIT for OFF-CYC hrs without 6 hr buckets : $fhr;;
         esac
         ln -fs "${freq}snow"  fort.17
         ln -sf "${freq}snowi" fort.18
         ln -sf "MAXMIN2"   fort.19
         ln -sf "MAXMIN1"   fort.20
         ln -sf "MAXMIN2i"  fort.21
         ln -sf "MAXMIN1i"  fort.22

       else           
#        READ PRECIP FROM INPUT MDL GRIB FILE 
#        ON-CYC:  Forecast hours 3,15,27,39....already  have 3-hr buckets,
#        OFF-CYC: 3 hour buckets available for all 3 hour forecast times
#        ALL-CYC: Input only  max/min temp data for the previous 2 hours

         echo "****************************************************"
         echo RUN SMARTINIT for hours with 3 hr buckets: $fhr
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
   conus|conusnest|dgex_cs) RGIN=CS;;
      conusnest2p5) RGIN=CS2P;hrlyfhr=36;;
        ak_rtmages) RGIN=AKRT;;
                 *) RGIN=`echo $rg |tr '[a-z]'  '[A-Z]' `;;
   esac

  export pgm=smartinit; . prep_step
  ${EXECdng}/smartinit $cyc $fhr $ogrd $RGIN $inest $inhrfrq $fhrstr $core >smartinit.out${fhr}
  export err=$?; err_chk

# Save hourly ak,hi,pr,conus2p5 nests and ak_rtmages(from nam parent) for RTMA 1st guess fields
  if [ $fhr -le $hrlyfhr ];then
    case $RUNTYP in
     ak_rtmages) 
       cp MESO${RGIN}${fhr}.tm00  $COMOUT/${mdl}.t${cyc}z.smart${RUNTYP}${fhr}.tm00
       mksmart=0;;
     hawaiinest|priconest|conusnest2p5|aknest3)
       cp MESO${RGIN}${fhr}.tm00  $COMOUT/${mdl}.t${cyc}z.smart${outreg}${fhr}.tm00;;
   esac
  fi

  if [ $mksmart -eq 1 ];then
#   Run NCO processing to convert output to grib2 and awips
    export RUNTYP
    export RGIN=$RGIN  # Region id (eg: CS, HI, PR,AK..)
    export outreg
    export cyc  
    export fhr=$fhr
    export ogrd 
    if [ $mdl = "hiresw" ];then
      ${USHdng}/dng_awp.sh $mdlgrd
    else
      awpchk=0
      ${USHdng}/dng_awp.sh $outreg $awpchk
    fi
  fi
done  #fhr loop

exit
