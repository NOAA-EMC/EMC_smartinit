c!/bin/ksh
#@ output = /meso/save/wx22mc/smart/nam_conussmart.out
#@ error = /meso/save/wx22mc/smart/nam_conussmart.out
#@ job_type = parallel
#@ class = devhigh
#@ group = devonprod
#@ total_tasks = 1
#@ resources = ConsumableCpus(1) ConsumableMemory(2 GB)
#@ account_no=NAM-T2O
#@ wall_clock_limit = 00:49:00
#@ network.MPI = csss,shared,us
#@ queue
#
# Author:        Geoff Manikin       Org: NP22         Date: 2007-08-06
#
# Script history log:
# 2007-08-06  Geoff Manikin
#

mkdir /stmp/wx22mc/smarttest
rm /stmp/wx22mc/smarttest/*
cd /stmp/wx22mc/smarttest

ffhr=12
ymd=20120614
ymds=20120614
cyc=12
set -x

set -a prcthrsh[01] 


if [ ${cyc} -eq 0 ]
then
 let srefcyc=21
else
let srefcyc=cyc-3
fi

let pcphr=ffhr+3
let pcphrl=ffhr+3
let pcphr12=pcphr-12
let pcphr6=pcphr-6
let pcphr3=pcphr-3

typeset -Z2 srefcyc
typeset -Z2 pcphrl

export FIXruc2=${FIXruc2:-/nwprod/fix}
export COMIN_SREF=/com/sref/prod/sref.${ymds}/${srefcyc}/ensprod/
export utilexec=/nwprod/util/exec
export COMIN=/com/nam/prod/nam.${ymd}
export EXECnam=/nwprod/exec
export PARMnam=/nwprod/parm
export FIXnam=/nwprod/fix
export COMOUT=/ptmp/wx22mc/smart

if [ $ffhr -gt 0 ]; then
# get the sref precip fields that we need
#cp $COMIN_SREF/sref.t${srefcyc}z.pgrb212.prob SREFPROB
if [ ${cyc} -eq 0 ]
then
 let srefcyc=18
else
let srefcyc=cyc-6
fi
let pcphr=ffhr+6
let pcphrl=ffhr+6
let pcphr12=pcphr-12
let pcphr6=pcphr-6
let pcphr3=pcphr-3
typeset -Z2 srefcyc pcphrl
cp /ptmp/wx20jd/com/sref/prod/sref.${ymds}/${srefcyc}/ensprod_gefs/sref.t${srefcyc}z.pgrb212.prob_3hrly SREFPROB

$utilexec/grbindex SREFPROB SREFPROBI

for pthrsh in 01 05 10 25 50;do  
# 3-hr prob of pcp > 0.01
$utilexec/wgrib -PDS10 SREFPROB |grep "2 0 0 0 0 ${pthrshindx[pthr]}"| grep "0 1 $pcphr3 $pcphr 4"|$utilexec/wgrib -i -grib -o dump SREFPROB
mv dump srefpcp1

# 3-hr prob of pcp > 0.05
$utilexec/wgrib -PDS10 SREFPROB |grep "2 0 0 0 0 65 20 81 236"| grep "0 1 $pcphr3 $pcphr 4"|$utilexec/wgrib -i -grib -o dump SREFPROB
mv dump srefpcp2

# 3-hr prob of pcp > 0.10
$utilexec/wgrib -PDS10 SREFPROB |grep "2 0 0 0 0 65 40 163 215"| grep "0 1 $pcphr3 $pcphr 4"|$utilexec/wgrib -i -grib -o dump SREFPROB
mv dump srefpcp3

# 3-hr prob of pcp > 0.25
$utilexec/wgrib -PDS10 SREFPROB |grep "2 0 0 0 0 65 101 153 154"| grep "0 1 $pcphr3 $pcphr 4"|$utilexec/wgrib -i -grib -o dump SREFPROB
mv dump srefpcp4

# 3-hr prob of pcp > 0.50
$utilexec/wgrib -PDS10 SREFPROB |grep "2 0 0 0 0 65 203 51 51"| grep "0 1 $pcphr3 $pcphr 4"|$utilexec/wgrib -i -grib -o dump SREFPROB
mv dump srefpcp5

if [ $ffhr -gt 5 ]; then
# 6-hr prob of pcp > 0.01
$utilexec/wgrib -PDS10 SREFPROB |grep "2 0 0 0 0 64 64 0 0"| grep "0 1 $pcphr6 $pcphr 4"|$utilexec/wgrib -i -grib -o dump SREFPROB
mv dump srefpcp6

# 6-hr prob of pcp > 0.05
$utilexec/wgrib -PDS10 SREFPROB |grep "2 0 0 0 0 65 20 81 236"| grep "0 1 $pcphr6 $pcphr 4"|$utilexec/wgrib -i -grib -o dump SREFPROB
mv dump srefpcp7

# 6-hr prob of pcp > 0.10
$utilexec/wgrib -PDS10 SREFPROB |grep "2 0 0 0 0 65 40 163 215"| grep "0 1 $pcphr6 $pcphr 4"|$utilexec/wgrib -i -grib -o dump SREFPROB
mv dump srefpcp8

# 6-hr prob of pcp > 0.25
$utilexec/wgrib -PDS10 SREFPROB |grep "2 0 0 0 0 65 101 153 154"| grep "0 1 $pcphr6 $pcphr 4"|$utilexec/wgrib -i -grib -o dump SREFPROB
mv dump srefpcp9

# 6-hr prob of pcp > 0.50
$utilexec/wgrib -PDS10 SREFPROB |grep "2 0 0 0 0 65 203 51 51"| grep "0 1 $pcphr6 $pcphr 4"|$utilexec/wgrib -i -grib -o dump SREFPROB
mv dump srefpcp10
fi

if [ $ffhr -gt 11 ]; then
# 12-hr prob of pcp > 0.01
$utilexec/wgrib -PDS10 SREFPROB |grep "2 0 0 0 0 64 64 0 0"| grep "0 1 $pcphr12 $pcphr 4"|$utilexec/wgrib -i -grib -o dump SREFPROB
mv dump srefpcp11

# 12-hr prob of pcp > 0.05
$utilexec/wgrib -PDS10 SREFPROB |grep "2 0 0 0 0 65 20 81 236"| grep "0 1 $pcphr12 $pcphr 4"|$utilexec/wgrib -i -grib -o dump SREFPROB
mv dump srefpcp12

# 12-hr prob of pcp > 0.10
$utilexec/wgrib -PDS10 SREFPROB |grep "2 0 0 0 0 65 40 163 215"| grep "0 1 $pcphr12 $pcphr 4"|$utilexec/wgrib -i -grib -o dump SREFPROB
mv dump srefpcp13

# 12-hr prob of pcp > 0.25
$utilexec/wgrib -PDS10 SREFPROB |grep "2 0 0 0 0 65 101 153 154"| grep "0 1 $pcphr12 $pcphr 4"|$utilexec/wgrib -i -grib -o dump SREFPROB
mv dump srefpcp14

# 12-hr prob of pcp > 0.50
$utilexec/wgrib -PDS10 SREFPROB |grep "2 0 0 0 0 65 203 51 51"| grep "0 1 $pcphr12 $pcphr 4"|$utilexec/wgrib -i -grib -o dump SREFPROB
mv dump srefpcp15
fi

if [ $ffhr -gt 11 ]; then
cat srefpcp1 srefpcp2 srefpcp3 srefpcp4 srefpcp5 srefpcp6 srefpcp7 srefpcp8 srefpcp9 srefpcp10 srefpcp11 srefpcp12 srefpcp13 srefpcp14 srefpcp15 > srefallpcp
elif [ $ffhr -gt 5 ]; then
cat srefpcp1 srefpcp2 srefpcp3 srefpcp4 srefpcp5 srefpcp6 srefpcp7 srefpcp8 srefpcp9 srefpcp10 > srefallpcp
elif [ $ffhr -gt 1 ]; then
cat srefpcp1 srefpcp2 srefpcp3 srefpcp4 srefpcp5 > srefallpcp
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

for fhr in $hours; do

  let check=fhr%3
  let fhr1=fhr-1
  let fhr2=fhr-2
  let fhr3=fhr-3
  let fhr6=fhr-6
  let fhr9=fhr-9

  typeset -Z2 fhr3
  typeset -Z2 fhr1
  typeset -Z2 fhr2
  typeset -Z2 fhr6
  typeset -Z2 fhr9
  typeset -Z2 fhr

  cp $COMIN/nam.t${cyc}z.bgrd3d${fhr}.tm00 WRFPRS${fhr}.tm00
  $utilexec/grbindex WRFPRS${fhr}.tm00 WRFPRS${fhr}i.tm00
  cp $PARMnam/nam_masterconus.ctl master${fhr}.ctl

  # if the forecast hour is a 3-hr time (3,15,27,39....) it already
  #   contains 3-hr accumulations;  otherwise, we need to make 3-hr
  #   accumulations.   We don't need them for in-between hours (22,23,25...),
  #   since we only gather max/min data at those hours

  if [ $fhr -ne 00 -a $fhr -ne 03 -a $fhr -ne 15 -a $fhr -ne 27 -a \
       $fhr -ne 39 -a $fhr -ne 51 -a $fhr -ne 63 -a $fhr -ne 75 ] ; then
    if [ ${check} -eq 0 ]; then
     cp $COMIN/nam.t${cyc}z.bgrd3d${fhr3}.tm00 WRFPRS${fhr3}.tm00
     $utilexec/grbindex WRFPRS${fhr3}.tm00 WRFPRS${fhr3}i.tm00

     export XLFUNIT_13="WRFPRS${fhr3}.tm00"
     export XLFUNIT_14="WRFPRS${fhr3}i.tm00"
     export XLFUNIT_15="WRFPRS${fhr}.tm00"
     export XLFUNIT_16="WRFPRS${fhr}i.tm00"
     export XLFUNIT_50="3precip.${fhr}"
     export XLFUNIT_51="3cprecip.${fhr}"
     export XLFUNIT_52="3snow.${fhr}"
$EXECnam/nam_smartmakeprecip <<EOF > makeprecip3.out
$fhr $fhr3
EOF

     grid="255 3 1073 689 20192 238446 8 265000 5079 5079 0 64 25000 25000"
     $utilexec/copygb -g "$grid" -i3 -x 3precip.${fhr} 3precip
     $utilexec/grbindex 3precip 3precipi
     $utilexec/copygb -g "$grid" -i3 -x 3snow.${fhr} 3snow
     $utilexec/grbindex 3snow 3snowi
    fi
  fi

  # at 12-hr "on" times, we need 6 hour buckets as well
  #  we don't need them at 6-hr times (18,30,42...) because
  #  those files already have 6-hr buckets

  if [ $fhr -eq 12 -o $fhr -eq 24 -o $fhr -eq 36 -o $fhr -eq 48 -o \
       $fhr -eq 60 -o $fhr -eq 72 -o $fhr -eq 84 ] ; then
  cp $COMIN/nam.t${cyc}z.bgrd3d${fhr6}.tm00 WRFPRS${fhr6}.tm00
  $utilexec/grbindex WRFPRS${fhr}.tm00 WRFPRS${fhr}i.tm00
  $utilexec/grbindex WRFPRS${fhr6}.tm00 WRFPRS${fhr6}i.tm00

    export XLFUNIT_13="WRFPRS${fhr6}.tm00"
    export XLFUNIT_14="WRFPRS${fhr6}i.tm00"
    export XLFUNIT_15="WRFPRS${fhr}.tm00"
    export XLFUNIT_16="WRFPRS${fhr}i.tm00"
    export XLFUNIT_50="6precip.${fhr}"
    export XLFUNIT_51="6cprecip.${fhr}"
    export XLFUNIT_52="6snow.${fhr}"
$EXECnam/nam_smartmakeprecip <<EOF > makeprecip6.out
$fhr $fhr6
EOF

  grid="255 3 1073 689 20192 238446 8 265000 5079 5079 0 64 25000 25000" 
  $utilexec/copygb -g "$grid" -i3 -x 6precip.${fhr} 6precip
  $utilexec/grbindex 6precip 6precipi
  $utilexec/copygb -g "$grid" -i3 -x 6snow.${fhr} 6snow
  $utilexec/grbindex 6snow 6snowi
 fi

cat >input${fhr}.prd <<EOF5
WRFPRS${fhr}.tm00
EOF5

  ln -sf master${fhr}.ctl                 fort.10
  ln -sf $FIXnam/nam_wgt_197         fort.21
  ln -sf $PARMnam/nam_kwbx.tbl       fort.41
  ln -sf $PARMnam/nam_time.tbl       fort.42
  ln -sf $PARMnam/nam_parm.tbl       fort.43
  ln -sf $PARMnam/nam_grid.tbl       fort.44
  ln -sf $PARMnam/nam_levl.tbl       fort.45
  $EXECnam/nam_prdgen < input${fhr}.prd > prdgen.out${fhr}

  cp /com/date/t${cyc}z DATE

    mv meso.NDFD mesocon.NDFDf${fhr}
    $utilexec/grbindex mesocon.NDFDf${fhr} mesocon.NDFDif${fhr}

    cp $FIXruc2/ruc2_ndfdtopo.dat ruc2_ndfdtopo.dat
    cp $FIXruc2/ruc2_vegtype_ndfd.dat ruc2_vegtype_ndfd.dat

  # at 12-hr times, we need to make 12-hr max/min temps
  #   we also need 3 and 6-hr buckets

  if [ $fhr -eq 12 -o $fhr -eq 24 -o $fhr -eq 36 -o $fhr -eq 48 -o \
       $fhr -eq 60 -o $fhr -eq 72 -o $fhr -eq 84 ] ; then
    cp srefpcpcon_${SREF_PDY}${srefcyc}f0${pcphrl} SREFPCP
    cp srefpcpconi_${SREF_PDY}${srefcyc}f0${pcphrl} SREFPCPi

    cp MAXMIN${fhr2}.tm00 MAXMIN2
    cp MAXMIN${fhr1}.tm00 MAXMIN1
    cp $COMOUT/nam.t${cyc}z.smartconus${fhr3}.tm00 MAXMIN3
    cp $COMOUT/nam.t${cyc}z.smartconus${fhr6}.tm00 MAXMIN4
    cp $COMOUT/nam.t${cyc}z.smartconus${fhr9}.tm00 MAXMIN5

    /nwprod/util/exec/grbindex MAXMIN1 MAXMIN1i
    /nwprod/util/exec/grbindex MAXMIN2 MAXMIN2i
    /nwprod/util/exec/grbindex MAXMIN3 MAXMIN3i
    /nwprod/util/exec/grbindex MAXMIN4 MAXMIN4i
    /nwprod/util/exec/grbindex MAXMIN5 MAXMIN5i

    export XLFUNIT_11="mesocon.NDFDf${fhr}"
    export XLFUNIT_12="mesocon.NDFDif${fhr}"
    export XLFUNIT_13="SREFPCP"
    export XLFUNIT_14="SREFPCPi"
    export XLFUNIT_15="3precip"
    export XLFUNIT_16="3precipi"
    export XLFUNIT_17="6precip"
    export XLFUNIT_18="6precipi"
    export XLFUNIT_19="3snow"
    export XLFUNIT_20="3snowi"
    export XLFUNIT_21="6snow"
    export XLFUNIT_22="6snowi" 
    export XLFUNIT_23="MAXMIN1"
    export XLFUNIT_24="MAXMIN2"
    export XLFUNIT_25="MAXMIN3"
    export XLFUNIT_26="MAXMIN4"
    export XLFUNIT_27="MAXMIN5"
    export XLFUNIT_28="MAXMIN1i"
    export XLFUNIT_29="MAXMIN2i"
    export XLFUNIT_30="MAXMIN3i"
    export XLFUNIT_31="MAXMIN4i"
    export XLFUNIT_32="MAXMIN5i"
    export XLFUNIT_46="$FIXruc2/ruc2_ndfdtopo.dat"
    export XLFUNIT_47="$FIXruc2/ruc2_vegtype_ndfd.dat"
    $EXECnam/nam_smartinitconus <<EOF >> smartinit.out${fhr}
$fhr
$cyc
EOF

mv MESOCS${fhr}.tm00 $COMOUT/nam.t${cyc}z.smartconus${fhr}.tm00

  # for all forecast hours divisible by 3 except for (3,15,27....), we need
  #    3-hr buckets and max/min temp data for the previous 2 hours

  elif [ $check -eq 0 -a $fhr -ne 00 -a $fhr -ne 03 -a $fhr -ne 15 -a $fhr -ne 27 -a \
       $fhr -ne 39 -a $fhr -ne 51 -a $fhr -ne 63 -a $fhr -ne 75 ] ; then

    cp srefpcpcon_${SREF_PDY}${srefcyc}f0${pcphrl} SREFPCP
    cp srefpcpconi_${SREF_PDY}${srefcyc}f0${pcphrl} SREFPCPi
    cp MAXMIN${fhr2}.tm00 MAXMIN2
    cp MAXMIN${fhr1}.tm00 MAXMIN1

    $utilexec/grbindex MAXMIN2 MAXMIN2i
    $utilexec/grbindex MAXMIN1 MAXMIN1i

    export XLFUNIT_11="mesocon.NDFDf${fhr}"
    export XLFUNIT_12="mesocon.NDFDif${fhr}"
    export XLFUNIT_13="SREFPCP"
    export XLFUNIT_14="SREFPCPi"
    export XLFUNIT_15="3precip"
    export XLFUNIT_16="3precipi"
    export XLFUNIT_17="3snow"
    export XLFUNIT_18="3snowi"
    export XLFUNIT_19="MAXMIN2"
    export XLFUNIT_20="MAXMIN1"
    export XLFUNIT_21="MAXMIN2i"
    export XLFUNIT_22="MAXMIN1i"
    export XLFUNIT_46="$FIXruc2/ruc2_ndfdtopo.dat"
    export XLFUNIT_47="$FIXruc2/ruc2_vegtype_ndfd.dat"
    $EXECnam/nam_smartinitconus <<EOF >> smartinit.out${fhr}
$fhr
$cyc
EOF
mv MESOCS${fhr}.tm00 $COMOUT/nam.t${cyc}z.smartconus${fhr}.tm00

  #  for forecast hours 3,15,27,39.... the files already have 3-hr buckets,
  #   but we need max/min temp data for the previous 2 hours

  elif [ ${check} -eq 0 -a ${fhr} -ne 0 ]; then
    cp srefpcpcon_${SREF_PDY}${srefcyc}f0${pcphrl} SREFPCP
    cp srefpcpconi_${SREF_PDY}${srefcyc}f0${pcphrl} SREFPCPi
    cp MAXMIN${fhr2}.tm00 MAXMIN2
    cp MAXMIN${fhr1}.tm00 MAXMIN1

    $utilexec/grbindex MAXMIN2 MAXMIN2i
    $utilexec/grbindex MAXMIN1 MAXMIN1i

    export XLFUNIT_11="mesocon.NDFDf${fhr}"
    export XLFUNIT_12="mesocon.NDFDif${fhr}"
    export XLFUNIT_13="SREFPCP"
    export XLFUNIT_14="SREFPCPi"
    export XLFUNIT_15="MAXMIN2"
    export XLFUNIT_16="MAXMIN1"
    export XLFUNIT_17="MAXMIN2i"
    export XLFUNIT_18="MAXMIN1i"
    export XLFUNIT_46="$FIXruc2/ruc2_ndfdtopo.dat"
    export XLFUNIT_47="$FIXruc2/ruc2_vegtype_ndfd.dat"
    $EXECnam/nam_smartinitconus <<EOF >> smartinit.out${fhr}
$fhr
$cyc
EOF
  
mv MESOCS${fhr}.tm00 $COMOUT/nam.t${cyc}z.smartconus${fhr}.tm00

  #  for all "in-between" forecast hours (13,14,16....), we don't need
  #    any special data
  else 
    export XLFUNIT_11="mesocon.NDFDf${fhr}"
    export XLFUNIT_12="mesocon.NDFDif${fhr}"
    export XLFUNIT_46="$FIXruc2/ruc2_ndfdtopo.dat"
    export XLFUNIT_47="$FIXruc2/ruc2_vegtype_ndfd.dat"
    $EXECnam/nam_smartinitconus <<EOF >> smartinit.out${fhr}
$fhr
$cyc
EOF
  if [ $fhr -eq 0 ] ; then
   mv MESOCS${fhr}.tm00 $COMOUT/nam.t${cyc}z.smartconus${fhr}.tm00
  fi
  fi
done
exit
