#!/bin/ksh --login

#BSUB -J smartinit
#BSUB -o out.smartinit%J
#BSUB -e err.smartinit%J
#BSUB -q hpc_ibm
#BSUB -n 1
#BSUB -W 01:00
####BSUB -R span[ptile=1]
#BSUB -x
#BSUB -a poe

export envir=dev
set -x


#
# Script history log:
# 2007-08-06  Geoff Manikin
#
udir=Jeff.McQueen
rdir=/meso/save/${udir}

mkdir /stmp/${udir}/smart
rm /stmp/${udir}/smart/*
cd /stmp/${udir}/smart

# Forecast hour (03,06,09 or 12) to create maxmin files
# must run shorter fhrs first to create maxmin files for fhr=12
ffhr=03


ymd=20121022
ymds=$ymd
cyc=12
set -x

if [ ${cyc} -eq 0 ]
then
 let srefcyc=21
else
let srefcyc=$cyc-3
fi

let pcphr=ffhr+3
let pcphrl=ffhr+3
let pcphr12=pcphr-12
let pcphr6=pcphr-6
let pcphr3=pcphr-3

typeset -Z2 srefcyc
typeset -Z2 pcphrl

export FIXruc2=${FIXruc2:-${rdir}/nwprod/fix}
export COMIN_SREF=/com_canned/sref/prod/sref.${ymds}/${srefcyc}/ensprod/
export utilexec=/nwprod/util/exec
export wgexec=${utilexec}
export COMIN=/com_canned/nam/prod/nam.${ymd}
export EXECnam=${rdir}/nwprod/sorc
export PARMnam=${rdir}/nwprod/parm
export FIXnam=${rdir}/nwprod/fix
export COMOUT=/ptmp/${udir}/smart

typeset -Z2 srefcyc pcphrl
if [ $ffhr -gt 0 ]; then
# get the sref precip fields that we need
#WCOSS CHANGE
cp $COMIN_SREF/sref.t${srefcyc}z.pgrb212.prob_3hrly SREFPROB
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

$utilexec/grbindex SREFPROB SREFPROBI

# 3-hr prob of pcp > 0.01
$utilexec/wgrib -PDS10 SREFPROB |grep "2 0 0 0 0 64 64 0 0"| grep "0 1 $pcphr3 $pcphr 4"|$utilexec/wgrib -i -grib -o dump SREFPROB
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

 ${wgexec}/wgrib -s WRFPRS${fhr}.tm00 | grep -f ${PARMnam}/nam_smartinit.parmlist | ${wgexec}/wgrib -i -grib -o temp WRFPRS${fhr}.tm00 > wgrib.out
  mv temp WRFPRS${fhr}.tm00

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

 ${wgexec}/wgrib -s WRFPRS${fhr3}.tm00 | grep -f ${PARMnam}/nam_smartinit.parmlist | ${wgexec}/wgrib -i -grib -o temp WRFPRS${fhr3}.tm00 > wgrib.out
  mv temp WRFPRS${fhr3}.tm00

     $utilexec/grbindex WRFPRS${fhr3}.tm00 WRFPRS${fhr3}i.tm00

     ln -fs "WRFPRS${fhr3}.tm00" fort.13
     ln -fs "WRFPRS${fhr3}i.tm00" fort.14
     ln -fs "WRFPRS${fhr}.tm00" fort.15 
     ln -fs "WRFPRS${fhr}i.tm00" fort.16
     ln -fs "3precip.${fhr}" fort.50
     ln -fs "3cprecip.${fhr}" fort.51
     ln -fs "3snow.${fhr}" fort.52
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

 ${wgexec}/wgrib -s WRFPRS${fhr6}.tm00 | grep -f ${PARMnam}/nam_smartinit.parmlist | ${wgexec}/wgrib -i -grib -o temp WRFPRS${fhr6}.tm00 > wgrib.out
  mv temp WRFPRS${fhr6}.tm00

  $utilexec/grbindex WRFPRS${fhr}.tm00 WRFPRS${fhr}i.tm00
  $utilexec/grbindex WRFPRS${fhr6}.tm00 WRFPRS${fhr6}i.tm00

    ln -fs "WRFPRS${fhr6}.tm00" fort.13
    ln -fs "WRFPRS${fhr6}i.tm00" fort.14
    ln -fs "WRFPRS${fhr}.tm00" fort.15
    ln -fs "WRFPRS${fhr}i.tm00" fort.16
    ln -fs "6precip.${fhr}" fort.50
    ln -fs "6cprecip.${fhr}" fort.51
    ln -fs "6snow.${fhr}" fort.52
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
  ln -sf input${fhr}.prd             fort.621
  $EXECnam/nam_prdgen.fd/nam_prdgen < input${fhr}.prd > prdgen.out${fhr}

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

    ln -fs "mesocon.NDFDf${fhr}" fort.11
    ln -fs "mesocon.NDFDif${fhr}" fort.12
    ln -fs "SREFPCP" fort.13
    ln -fs "SREFPCPi" fort.14
    ln -fs "3precip" fort.15
    ln -fs "3precipi" fort.16
    ln -fs "6precip" fort.17
    ln -fs "6precipi" fort.18
    ln -fs "3snow" fort.19
    ln -fs "3snowi" fort.20
    ln -fs "6snow" fort.21
    ln -fs "6snowi" fort.22
    ln -fs "MAXMIN1" fort.23
    ln -fs "MAXMIN2" fort.24
    ln -fs "MAXMIN3" fort.25
    ln -fs "MAXMIN4" fort.26
    ln -fs "MAXMIN5" fort.27
    ln -fs "MAXMIN1i" fort.28
    ln -fs "MAXMIN2i" fort.29
    ln -fs "MAXMIN3i" fort.30
    ln -fs "MAXMIN4i" fort.31
    ln -fs "MAXMIN5i" fort.32
    ln -fs "$FIXruc2/ruc2_ndfdtopo.dat" fort.46
    ln -fs "$FIXruc2/ruc2_vegtype_ndfd.dat" fort.47
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

    ln -fs "mesocon.NDFDf${fhr}" fort.11
    ln -fs "mesocon.NDFDif${fhr}" fort.12
    ln -fs "SREFPCP" fort.13
    ln -fs "SREFPCPi" fort.14
    ln -fs "3precip" fort.15
    ln -fs "3precipi" fort.16
    ln -fs "3snow" fort.17
    ln -fs "3snowi" fort.18
    ln -fs "MAXMIN2" fort.19
    ln -fs "MAXMIN1" fort.20
    ln -fs "MAXMIN2i" fort.21
    ln -fs "MAXMIN1i" fort.22
    ln -fs "$FIXruc2/ruc2_ndfdtopo.dat" fort.46
    ln -fs "$FIXruc2/ruc2_vegtype_ndfd.dat" fort.47
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

    ln -fs "mesocon.NDFDf${fhr}" fort.11
    ln -fs "mesocon.NDFDif${fhr}" fort.12
    ln -fs "SREFPCP" fort.13
    ln -fs "SREFPCPi" fort.14
    ln -fs "MAXMIN2" fort.15
    ln -fs "MAXMIN1" fort.16
    ln -fs "MAXMIN2i" fort.17
    ln -fs "MAXMIN1i" fort.18
    ln -fs "$FIXruc2/ruc2_ndfdtopo.dat" fort.46
    ln -fs "$FIXruc2/ruc2_vegtype_ndfd.dat" fort.47
    $EXECnam/nam_smartinitconus <<EOF >> smartinit.out${fhr}
$fhr
$cyc
EOF
  
mv MESOCS${fhr}.tm00 $COMOUT/nam.t${cyc}z.smartconus${fhr}.tm00

  #  for all "in-between" forecast hours (13,14,16....), we don't need
  #    any special data
  else 
    ln -fs "mesocon.NDFDf${fhr}" fort.11
    ln -fs "mesocon.NDFDif${fhr}" fort.12
    ln -fs "$FIXruc2/ruc2_ndfdtopo.dat" fort.46
    ln -fs "$FIXruc2/ruc2_vegtype_ndfd.dat" fort.47
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
