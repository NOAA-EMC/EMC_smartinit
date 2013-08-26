#!/bin/ksh
# #@ output = /meso/save/wx22mc/smart/nam_conussmart.out
# #@ error = /meso/save/wx22mc/smart/nam_conussmart.out
# #@ job_type = parallel
# #@ class = class1
# #@ group = dev
# #@ total_tasks = 1
# #@ resources = ConsumableCpus(1) ConsumableMemory(2 GB)
# #@ account_no=NAM-T2O
# #@ wall_clock_limit = 00:49:00
# #@ network.MPI = csss,shared,us
# #@ queue
#
# Author:        Geoff Manikin       Org: NP22         Date: 2007-08-06
#
# Script history log:
# 2007-08-06  Geoff Manikin
# 2012-07-24  Jeff McQueen  cleaned up redundant codes
#    Created precip threshold loop for creating sref prob precip files
#

mkdir /stmp/wx22mc/smarttest
#TEST rm /stmp/wx22mc/smarttest/*
cd /stmp/wx22mc/smarttest


#  Set Defaults pcp hours and frequencies
ffhr=12

ymd=20120805
ymds=$ymd
cyc=12
set -x

let srefcyc=cyc-3
if [ ${cyc} -eq 0 ];then let srefcyc=21;fi

#  Set Defaults pcp hours and frequencies
let pcphr=ffhr+3
let pcphrl=ffhr+3
let pcphr12=pcphr-12
let pcphr6=pcphr-6
let pcphr3=pcphr-3

typeset -Z2 srefcyc pcphrl

export FIXruc2=${FIXruc2:-/nwprod/fix}
export COMIN_SREF=/com/sref/prod/sref.${ymds}/${srefcyc}/ensprod/
export utilexec=/nwprod/util/exec
export COMIN=/com/nam/prod/nam.${ymd}
export EXECnam=/nwprod/exec
export PARMnam=/nwprod/parm
export FIXnam=/nwprod/fix
export COMOUT=/ptmp/wx22mc/smarttest


#======================================================================
#  CREATE SREF PROB. PRECIP FILES
#======================================================================

if [ $ffhr -gt 0 ]; then

# get the sref precip fields that we need
  cp $COMIN_SREF/sref.t${srefcyc}z.pgrb212.prob SREFPROB

#  Set Defaults pcp hours and frequencies for 6 hour intervals
 let srefcyc=cyc-6
 if [ ${cyc} -eq 0 ];then let srefcyc=18;fi 
  let pcphr=ffhr+6
  let pcphrl=ffhr+6
  let pcphr12=pcphr-12
  let pcphr6=pcphr-6
  let pcphr3=pcphr-3


  $utilexec/grbindex SREFPROB SREFPROBI
 
  let IP=0
  if [ $ffhr -lt 6 ]; then pcphr6=;pcphr12=;fi
  if [ $ffhr -lt 12 ]; then pcphr12=;fi
  for PHR in $pcphr3 $pcphr6 $pcphr12;do 

#   prob of pcp > 0.01
    $utilexec/wgrib -PDS10 SREFPROB |grep "2 0 0 0 0 64 64 0 0"| grep "0 1 $PHR $pcphr 4"|$utilexec/wgrib -i -grib -o dump SREFPROB
    let IP=IP+1
    mv dump srefpcp$IP

#   prob of pcp > 0.05
    $utilexec/wgrib -PDS10 SREFPROB |grep "2 0 0 0 0 65 20 81 236"| grep "0 1 $PHR $pcphr 4"|$utilexec/wgrib -i -grib -o dump SREFPROB
    let IP=IP+1
    mv dump srefpcp$IP

#   prob of pcp > 0.10
    $utilexec/wgrib -PDS10 SREFPROB |grep "2 0 0 0 0 65 40 163 215"| grep "0 1 $PHR $pcphr 4"|$utilexec/wgrib -i -grib -o dump SREFPROB
    let IP=IP+1
    mv dump srefpcp$IP

#   prob of pcp > 0.25
    $utilexec/wgrib -PDS10 SREFPROB |grep "2 0 0 0 0 65 101 153 154"| grep "0 1 $PHR $pcphr 4"|$utilexec/wgrib -i -grib -o dump SREFPROB
    let IP=IP+1
    mv dump srefpcp$IP

#   prob of pcp > 0.50
    $utilexec/wgrib -PDS10 SREFPROB |grep "2 0 0 0 0 65 203 51 51"| grep "0 1 $PHR $pcphr 4"|$utilexec/wgrib -i -grib -o dump SREFPROB
    let IP=IP+1
    mv dump srefpcp$IP
  done

  cat srefpcp1 srefpcp2 srefpcp3 srefpcp4 srefpcp5 > srefallpcp
  if [ $ffhr -ge 6 ]; then
    cat srefpcp6 srefpcp7 srefpcp8 srefpcp9 srefpcp10 >> srefallpcp
  fi
  if [ $ffhr -ge 12 ]; then
    cat srefpcp11 srefpcp12 srefpcp13 srefpcp14 srefpcp15 >> srefallpcp
  fi

  grid="255 3 1073 689 20192 238446 8 265000 5079 5079 0 64 25000 25000"
  $utilexec/copygb -g "$grid" -x srefallpcp srefpcpcon_${SREF_PDY}${srefcyc}f0${pcphrl}
  $utilexec/grbindex srefpcpcon_${SREF_PDY}${srefcyc}f0${pcphrl} srefpcpconi_${SREF_PDY}${srefcyc}f0${pcphrl}

fi

let ffhr1=ffhr-1
let ffhr2=ffhr-2

if [ $ffhr -gt 0 ]; then
  hours="${ffhr2} ${ffhr1} ${ffhr}"
else
  hours="00"
fi

#===========================================================
#  CREATE Accum precip buckets if necessary 
#===========================================================
for fhr in $hours; do

  let check=fhr%3
  let fhr1=fhr-1
  let fhr2=fhr-2
  let fhr3=fhr-3
  let fhr6=fhr-6
  let fhr9=fhr-9

  typeset -Z2 fhr1 fhr2 fhr3 fhr6 fhr9 fhr
  cp $COMIN/nam.t${cyc}z.bgrd3d${fhr}.tm00 WRFPRS${fhr}.tm00
  $utilexec/grbindex WRFPRS${fhr}.tm00 WRFPRS${fhr}i.tm00
  cp $PARMnam/nam_masterconus.ctl master${fhr}.ctl

# If 3-hr fhrs (3,15,27,39....) --> Already have 3-hr buckets
# For in-between fhrs (22,23,25) --> Create 3-hr buckets
# since we only gather max/min data at those hours
  inbethr=1
  if [ $fhr -ne 00 -a $fhr -ne 03 -a $fhr -ne 15 -a $fhr -ne 27 -a \
       $fhr -ne 39 -a $fhr -ne 51 -a $fhr -ne 63 -a $fhr -ne 75 ] ; then
    FHRFRQ=$fhr3;freq=3 
    inbethr=0
  fi
# at 12-hr "on" times:  Need 6 hour buckets as well
# 6 hr off-times (18,30,42...) : Already have 6 hour buckets
  if [ $fhr -eq 12 -o $fhr -eq 24 -o $fhr -eq 36 -o $fhr -eq 48 -o \
       $fhr -eq 60 -o $fhr -eq 72 -o $fhr -eq 84 ] ; then
    FHRFRQ=$fhr6;freq=6
    inbethr=0
  fi

  if [ ${check} -eq 0 -a ${inbethr} -eq 0 ]; then
    cp $COMIN/nam.t${cyc}z.bgrd3d${FHRFRQ}.tm00 WRFPRS${FHRFRQ}.tm00
    $utilexec/grbindex WRFPRS${FHRFRQ}.tm00 WRFPRS${FHRFRQ}i.tm00
#TEST    if [ $freq -eq 6 ];then
      $utilexec/grbindex WRFPRS${fhr}.tm00 WRFPRS${fhr}i.tm00
#TEST    fi

    ln -sf "WRFPRS${FHRFRQ}.tm00"  fort.13
    ln -sf "WRFPRS${FHRFRQ}i.tm00" fort.14
    ln -sf "WRFPRS${fhr}.tm00"     fort.15
    ln -sf "WRFPRS${fhr}i.tm00"    fort.16
    ln -sf "${freq}precip.${fhr}"  fort.50
    ln -sf "${freq}cprecip.${fhr}" fort.51
    ln -sf "${freq}snow.${fhr}"    fort.52
#===============================================================
# nam_smartmakeprecip : Create Precip Buckets for smartinit
#===============================================================
    $EXECnam/nam_smartmakeprecip <<EOF > makeprecip${freq}.out
$fhr $FHRFRQ
EOF

    grid="255 3 1073 689 20192 238446 8 265000 5079 5079 0 64 25000 25000"
    $utilexec/copygb -g "$grid" -i3 -x ${freq}precip.${fhr} ${freq}precip
    $utilexec/grbindex ${freq}precip ${freq}precipi
    $utilexec/copygb -g "$grid" -i3 -x ${freq}snow.${fhr} ${freq}snow
    $utilexec/grbindex ${freq}snow ${freq}snowi
  fi


#=================================================================
#  RUN PRODUCT GENERATOR
#=================================================================
cat >input${fhr}.prd <<EOF5
WRFPRS${fhr}.tm00
EOF5

  ln -sf master${fhr}.ctl            fort.10
  ln -sf $FIXnam/nam_wgt_197         fort.21
  ln -sf $PARMnam/nam_kwbx.tbl       fort.41
  ln -sf $PARMnam/nam_time.tbl       fort.42
  ln -sf $PARMnam/nam_parm.tbl       fort.43
  ln -sf $PARMnam/nam_grid.tbl       fort.44
  ln -sf $PARMnam/nam_levl.tbl       fort.45
  $EXECnam/nam_prdgen < input${fhr}.prd > prdgen.out${fhr}

  cp /com/date/t${cyc}z DATE


#=================================================================
#   DECLARE INPUTS and RUN SMARTINIT 
#=================================================================
  mv meso.NDFD mesocon.NDFDf${fhr}
  $utilexec/grbindex mesocon.NDFDf${fhr} mesocon.NDFDif${fhr}

  cp $FIXruc2/ruc2_ndfdtopo.dat ruc2_ndfdtopo.dat
  cp $FIXruc2/ruc2_vegtype_ndfd.dat ruc2_vegtype_ndfd.dat
  ln -sf "$FIXruc2/ruc2_ndfdtopo.dat"     fort.46
  ln -sf "$FIXruc2/ruc2_vegtype_ndfd.dat" fort.47

  if [ $check -eq 0 ];then 
    inbethr=0
    cp srefpcpcon_${SREF_PDY}${srefcyc}f0${pcphrl} SREFPCP
    cp srefpcpconi_${SREF_PDY}${srefcyc}f0${pcphrl} SREFPCPi
    cp MAXMIN${fhr2}.tm00 MAXMIN2
    cp MAXMIN${fhr1}.tm00 MAXMIN1
    /nwprod/util/exec/grbindex MAXMIN1 MAXMIN1i
    /nwprod/util/exec/grbindex MAXMIN2 MAXMIN2i
  fi

  ln -sf "mesocon.NDFDf${fhr}"      fort.11
  ln -sf "mesocon.NDFDif${fhr}"     fort.12
  ln -sf "SREFPCP"                  fort.13
  ln -sf "SREFPCPi"                 fort.14
  ln -sf "3precip"                  fort.15
  ln -sf "3precipi"                 fort.16

# At 12-hr times, input 12-hr max/min temps and 3 and 6-hr buckets
  if [ $fhr -eq 12 -o $fhr -eq 24 -o $fhr -eq 36 -o $fhr -eq 48 -o \
       $fhr -eq 60 -o $fhr -eq 72 -o $fhr -eq 84 ] ; then

    cp $COMOUT/nam.t${cyc}z.smartconus${fhr3}.tm00 MAXMIN3
    cp $COMOUT/nam.t${cyc}z.smartconus${fhr6}.tm00 MAXMIN4
    cp $COMOUT/nam.t${cyc}z.smartconus${fhr9}.tm00 MAXMIN5

    /nwprod/util/exec/grbindex MAXMIN3 MAXMIN3i
    /nwprod/util/exec/grbindex MAXMIN4 MAXMIN4i
    /nwprod/util/exec/grbindex MAXMIN5 MAXMIN5i

    ln -sf "6precip"   fort.17
    ln -sf "6precipi"  fort.18
    ln -sf "3snow"     fort.19
    ln -sf "3snowi"    fort.20
    ln -sf "6snow"     fort.21
    ln -sf "6snowi"    fort.22
    ln -sf "MAXMIN1"   fort.23
    ln -sf "MAXMIN2"   fort.24
    ln -sf "MAXMIN3"   fort.25
    ln -sf "MAXMIN4"   fort.26
    ln -sf "MAXMIN5"   fort.27
    ln -sf "MAXMIN1i"  fort.28
    ln -sf "MAXMIN2i"  fort.29
    ln -sf "MAXMIN3i"  fort.30
    ln -sf "MAXMIN4i"  fort.31
    ln -sf "MAXMIN5i"  fort.32

# For all forecast hours divisible by 3 except for (3,15,27....), 
# input 3-hr buckets and max/min temp data for the previous 2 hours
  elif [ $check -eq 0 -a $fhr -ne 00 -a $fhr -ne 03 -a $fhr -ne 15 -a $fhr -ne 27 -a \
       $fhr -ne 39 -a $fhr -ne 51 -a $fhr -ne 63 -a $fhr -ne 75 ] ; then

    ln -fs "3snow"     fort.17
    ln -sf "3snowi"    fort.18
    ln -sf "MAXMIN2"   fort.19
    ln -sf "MAXMIN1"   fort.20
    ln -sf "MAXMIN2i"  fort.21
    ln -sf "MAXMIN1i"  fort.22

# For forecast hours 3,15,27,39.... the files already have 3-hr buckets,
# input max/min temp data for the previous 2 hours
  elif [ ${check} -eq 0 -a ${fhr} -ne 0 ]; then
    ln -sf "MAXMIN2"   fort.15
    ln -sf "MAXMIN1"   fort.16
    ln -sf "MAXMIN2i"  fort.17
    ln -sf "MAXMIN1i"  fort.18

# for all "in-between" forecast hours (13,14,16....)
# No special data needed
  else
    inbethr=1
#CCS    export XLFUNIT_13=;XLFUNIT_14=;XLFUNIT_15=;XLFUNIT_16=;
  fi

#========================================================
# Run Smartinit
#========================================================
  $EXECnam/nam_smartinitconus <<EOF >>smartinit.out${fhr}
$fhr
$cyc
EOF


  if [ $inbethr -eq 0 -o $inbethr -eq 1 -a $fhr -eq 0 ]; then 
    mv MESOCS${fhr}.tm00 $COMOUT/nam.t${cyc}z.smartconus${fhr}.tm00
  fi
done
exit
