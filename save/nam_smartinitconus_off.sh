#!/bin/ksh

#BSUB -oo out.smartinit
#BSUB -eo err.smartinit
#BSUB -n 1
#BSUB -J smartinittest
#BSUB -W 02:00
#BSUB -q "hpc_ibm"
####BSUB -R span[ptile=1]
#BSUB -x
#BSUB -a poe

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         nam_smartinitak_off.sh
# Script description:  runs smartinit code over NDFD conus domain
#
# Author:        Geoff Manikin       Org: NP22         Date: 2007-08-06
#
# Script history log:
# 2007-08-06  Geoff Manikin
#

set -x 

rdir=/u/Jeff.McQueen

# test=tst
 test=;

NCO=0

#mkdir -p $rdir/stmp/smart${test}
#rm $rdir/stmp/smart${test}/*
cd $rdir/stmp/smart${test}

#  Set Defaults fcst hours and frequencies
ffhr=12

ymd=20121017
ymds=$ymd
cyc=06
cycon=0
case $cyc in
   00|12 ) cycon=1;;
esac

let srefcyc=cyc-3
if [ ${cyc} -eq 0 ];then let srefcyc=21;fi

let pcphr=ffhr+3
let pcphrl=ffhr+3
let pcphr12=pcphr-12
let pcphr6=pcphr-6
let pcphr3=pcphr-3

typeset -Z2 srefcyc 
typeset -Z2 gefscyc
typeset -Z2 pcphrl

export FIXruc2=${FIXruc2:-${rdir}/nwprod/fix}
### cd $DATA
export FIXruc2=${FIXruc2:-${rdir}/nwprod/fix}
export COMIN_SREF=${rdir}/com/sref/prod/sref.${ymds}/${srefcyc}/ensprod/
export COMIN=${rdir}/com/nam/prod/nam.${ymd}
#TEST export utilexec=/nwprod/util/exec
export utilexec=/u/Ratko.Vasic/bin
export wgexec=/u/George.Vandenberghe/nwprod/util/exec
export EXECnam=/u/Eric.Rogers/nwprod/sorc/nam_prdgen.fd
export EXECsma=${rdir}/nwprod/sorc
export PARMnam=${rdir}/nwprod/parm
export FIXnam=/u/Eric.Rogers/nwprod/fix
export COMOUT=${rdir}/ptmp/smart${test}


if [ $ffhr -gt 0 ]; then
# get the sref precip fields that we need
cp $COMIN_SREF/sref.t${srefcyc}z.pgrb212.prob_3hrly SREFPROB

if [ ! -s SREFPROB ]; then
cp $COMIN_GEFS/${gefscyc}/sref.t${gefscyc}z.pgrb212.prob_3hrly SREFPROB
fi

$utilexec/grbindex SREFPROB SREFPROBI

# 3-hr prob of pcp > 0.01
$wgexec/wgrib -PDS10 SREFPROB |grep "2 0 0 0 0 64 64 0 0"| grep "0 1 $pcphr3 $pcphr 4"|$wgexec/wgrib -i -grib -o dump SREFPROB
mv dump srefpcp1

# 3-hr prob of pcp > 0.05
$wgexec/wgrib -PDS10 SREFPROB |grep "2 0 0 0 0 65 20 81 236"| grep "0 1 $pcphr3 $pcphr 4"|$wgexec/wgrib -i -grib -o dump SREFPROB
mv dump srefpcp2

# 3-hr prob of pcp > 0.10
$wgexec/wgrib -PDS10 SREFPROB |grep "2 0 0 0 0 65 40 163 215"| grep "0 1 $pcphr3 $pcphr 4"|$wgexec/wgrib -i -grib -o dump SREFPROB
mv dump srefpcp3

# 3-hr prob of pcp > 0.25
$wgexec/wgrib -PDS10 SREFPROB |grep "2 0 0 0 0 65 101 153 154"| grep "0 1 $pcphr3 $pcphr 4"|$wgexec/wgrib -i -grib -o dump SREFPROB
mv dump srefpcp4

# 3-hr prob of pcp > 0.50
$wgexec/wgrib -PDS10 SREFPROB |grep "2 0 0 0 0 65 203 51 51"| grep "0 1 $pcphr3 $pcphr 4"|$wgexec/wgrib -i -grib -o dump SREFPROB
mv dump srefpcp5

if [ $ffhr -gt 5 ]; then
# 6-hr prob of pcp > 0.01
$wgexec/wgrib -PDS10 SREFPROB |grep "2 0 0 0 0 64 64 0 0"| grep "0 1 $pcphr6 $pcphr 4"|$wgexec/wgrib -i -grib -o dump SREFPROB
mv dump srefpcp6

# 6-hr prob of pcp > 0.05
$wgexec/wgrib -PDS10 SREFPROB |grep "2 0 0 0 0 65 20 81 236"| grep "0 1 $pcphr6 $pcphr 4"|$wgexec/wgrib -i -grib -o dump SREFPROB
mv dump srefpcp7

# 6-hr prob of pcp > 0.10
$wgexec/wgrib -PDS10 SREFPROB |grep "2 0 0 0 0 65 40 163 215"| grep "0 1 $pcphr6 $pcphr 4"|$wgexec/wgrib -i -grib -o dump SREFPROB
mv dump srefpcp8

# 6-hr prob of pcp > 0.25
$wgexec/wgrib -PDS10 SREFPROB |grep "2 0 0 0 0 65 101 153 154"| grep "0 1 $pcphr6 $pcphr 4"|$wgexec/wgrib -i -grib -o dump SREFPROB
mv dump srefpcp9

# 6-hr prob of pcp > 0.50
$wgexec/wgrib -PDS10 SREFPROB |grep "2 0 0 0 0 65 203 51 51"| grep "0 1 $pcphr6 $pcphr 4"|$wgexec/wgrib -i -grib -o dump SREFPROB
mv dump srefpcp10
fi

if [ $ffhr -gt 11 ]; then
# 12-hr prob of pcp > 0.01
$wgexec/wgrib -PDS10 SREFPROB |grep "2 0 0 0 0 64 64 0 0"| grep "0 1 $pcphr12 $pcphr 4"|$wgexec/wgrib -i -grib -o dump SREFPROB
mv dump srefpcp11

# 12-hr prob of pcp > 0.05
$wgexec/wgrib -PDS10 SREFPROB |grep "2 0 0 0 0 65 20 81 236"| grep "0 1 $pcphr12 $pcphr 4"|$wgexec/wgrib -i -grib -o dump SREFPROB
mv dump srefpcp12

# 12-hr prob of pcp > 0.10
$wgexec/wgrib -PDS10 SREFPROB |grep "2 0 0 0 0 65 40 163 215"| grep "0 1 $pcphr12 $pcphr 4"|$wgexec/wgrib -i -grib -o dump SREFPROB
mv dump srefpcp13

# 12-hr prob of pcp > 0.25
$wgexec/wgrib -PDS10 SREFPROB |grep "2 0 0 0 0 65 101 153 154"| grep "0 1 $pcphr12 $pcphr 4"|$wgexec/wgrib -i -grib -o dump SREFPROB
mv dump srefpcp14

# 12-hr prob of pcp > 0.50
$wgexec/wgrib -PDS10 SREFPROB |grep "2 0 0 0 0 65 203 51 51"| grep "0 1 $pcphr12 $pcphr 4"|$wgexec/wgrib -i -grib -o dump SREFPROB
mv dump srefpcp15
fi

if [ $ffhr -gt 11 ]; then
cat srefpcp1 srefpcp2 srefpcp3 srefpcp4 srefpcp5 srefpcp6 srefpcp7 srefpcp8 srefpcp9 srefpcp10 srefpcp11 srefpcp12 srefpcp13 srefpcp14 srefpcp15 > srefallpcp
elif [ $ffhr -gt 5 ]; then
cat srefpcp1 srefpcp2 srefpcp3 srefpcp4 srefpcp5 srefpcp6 srefpcp7 srefpcp8 srefpcp9 srefpcp10 > srefallpcp
else
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

let fhr1=fhr-1
let fhr2=fhr-2
let fhr3=fhr-3
let fhr6=fhr-6
let fhr9=fhr-9
let check=fhr%3
let check6=fhr%6

typeset -Z2 fhr1
typeset -Z2 fhr2
typeset -Z2 fhr3
typeset -Z2 fhr6
typeset -Z2 fhr9
typeset -Z2 fhr

rm *.out${fhr}


 if [ -s WRFPRS${fhr}.tm00 ];then
  echo WRFPRS file found $fhr
 else
cp $COMIN/nam.t${cyc}z.bgrd3d${fhr}.tm00 WRFPRS${fhr}.tm00
  ${wgexec}/wgrib -s WRFPRS${fhr}.tm00 | grep -f ${PARMnam}/nam_smartinit.parmlist | ${wgexec}/wgrib -i -grib -o temp WRFPRS${fhr}.tm00 > wgrib.out
  mv temp WRFPRS${fhr}.tm00
  rm -f WRFPRS${fhr}i.tm00
 fi 

$utilexec/grbindex WRFPRS${fhr}.tm00 WRFPRS${fhr}i.tm00
cp $PARMnam/nam_masterconus.ctl master${fhr}.ctl

# if the forecast hour is valid at 00/12Z, we need to make 12-hr
#  accumulations.

if [ $fhr -eq 18 -o $fhr -eq 30 -o $fhr -eq 42 -o $fhr -eq 54 -o \
       $fhr -eq 66 -o $fhr -eq 78 ] ; then
 if [ -s WRFPRS${fhr3}.tm00 ];then
  echo WRFPRS file found $fhr3
 else
cp $COMIN/nam.t${cyc}z.bgrd3d${fhr3}.tm00 WRFPRS${fhr3}.tm00
$utilexec/grbindex WRFPRS${fhr3}.tm00 WRFPRS${fhr3}i.tm00
        ${wgexec}/wgrib -s WRFPRS${fhr3}.tm00 | grep -f ${PARMnam}/nam_smartinit.parmlist | ${wgexec}/wgrib -i -grib -o temp WRFPRS${fhr3}.tm00 > wgrib.out
        mv temp WRFPRS${fhr3}.tm00
        rm -f WRFPRS${fhr3}i.tm00
 fi
 if [ -s WRFPRS${fhr6}.tm00 ];then
  echo WRFPRS file found $fhr6
 else
cp $COMIN/nam.t${cyc}z.bgrd3d${fhr6}.tm00 WRFPRS${fhr6}.tm00
       ${wgexec}/wgrib -s WRFPRS${fhr6}.tm00 | grep -f ${PARMnam}/nam_smartinit.parmlist | ${wgexec}/wgrib -i -grib -o temp WRFPRS${fhr6}.tm00 > wgrib.out
        mv temp WRFPRS${fhr6}.tm00
        rm -f WRFPRS${fhr6}i.tm00 
$utilexec/grbindex WRFPRS${fhr6}.tm00 WRFPRS${fhr6}i.tm00
 fi
 if [ -s WRFPRS${fhr9}.tm00 ];then
  echo WRFPRS file found $fhr9
 else
cp $COMIN/nam.t${cyc}z.bgrd3d${fhr9}.tm00 WRFPRS${fhr9}.tm00
       ${wgexec}/wgrib -s WRFPRS${fhr9}.tm00 | grep -f ${PARMnam}/nam_smartinit.parmlist | ${wgexec}/wgrib -i -grib -o temp WRFPRS${fhr9}.tm00 > wgrib.out
        mv temp WRFPRS${fhr9}.tm00
        rm -f WRFPRS${fhr9}i.tm00
$utilexec/grbindex WRFPRS${fhr9}.tm00 WRFPRS${fhr9}i.tm00
 fi
    export pgm=nam_smartaddprecip12;
#NCO . prep_step
    ln -fs "WRFPRS${fhr9}.tm00" fort.13
    ln -fs "WRFPRS${fhr9}i.tm00" fort.14
    ln -fs "WRFPRS${fhr6}.tm00" fort.15
    ln -fs "WRFPRS${fhr6}i.tm00" fort.16
    ln -fs "WRFPRS${fhr3}.tm00" fort.17
    ln -fs "WRFPRS${fhr3}i.tm00" fort.18
    ln -fs "WRFPRS${fhr}.tm00" fort.19
    ln -fs "WRFPRS${fhr}i.tm00" fort.20
    ln -fs "12precip.${fhr}" fort.50
    ln -fs "12cprecip.${fhr}" fort.51
    ln -fs "12snow.${fhr}" fort.52
    $EXECsma/nam_smartaddprecip12 <<EOF > addprecip12.out
$fhr9 $fhr6 $fhr3 $fhr
EOF
export err=$?;
#NCO err_chk

grid="255 3 1073 689 20192 238446 8 265000 5079 5079 0 64 25000 25000"
$utilexec/copygb -g "$grid" -i3 -x 12precip.${fhr} 12precip
$utilexec/grbindex 12precip 12precipi
$utilexec/copygb -g "$grid" -i3 -x 12snow.${fhr} 12snow
$utilexec/grbindex 12snow 12snowi
fi

# at all 6-hour intervals, we need 6 hour buckets as well
if [ $check6 -eq 0 -a $fhr -ne 0 ] ; then
 echo 'need 6-hr buckets'
  if [ -s WRFPRS${fhr3}.tm00 ];then
  echo WRFPRS file found $fhr3
  else

cp $COMIN/nam.t${cyc}z.bgrd3d${fhr3}.tm00 WRFPRS${fhr3}.tm00
      ${wgexec}/wgrib -s WRFPRS${fhr3}.tm00 | grep -f ${PARMnam}/nam_smartinit.parmlist | ${wgexec}/wgrib -i -grib -o temp WRFPRS${fhr3}.tm00 > wgrib.out
        mv temp WRFPRS${fhr3}.tm00
        rm -f WRFPRS${fhr3}i.tm00
  fi
$utilexec/grbindex WRFPRS${fhr}.tm00 WRFPRS${fhr}i.tm00
$utilexec/grbindex WRFPRS${fhr3}.tm00 WRFPRS${fhr3}i.tm00

    export pgm=nam_smartaddprecip6;
# NCO. prep_step 
    ln -fs WRFPRS${fhr3}.tm00  fort.13
    ln -fs WRFPRS${fhr3}i.tm00 fort.14
    ln -fs WRFPRS${fhr}.tm00   fort.15
    ln -fs WRFPRS${fhr}i.tm00  fort.16
    ln -fs 6precip.${fhr}      fort.50
    ln -fs 6cprecip.${fhr}     fort.51
    ln -fs 6snow.${fhr}        fort.52

    $EXECsma/nam_smartaddprecip6 <<EOF > addprecip6.out
$fhr3 $fhr
EOF
export err=$?;
# NCO err_chk


grid="255 3 1073 689 20192 238446 8 265000 5079 5079 0 64 25000 25000"
$utilexec/copygb -g "$grid" -i3 -x 6precip.${fhr} 6precip
$utilexec/grbindex 6precip 6precipi
$utilexec/copygb -g "$grid" -i3 -x 6snow.${fhr} 6snow
$utilexec/grbindex 6snow 6snowi
fi

cat >input${fhr}.prd <<EOF5
WRFPRS${fhr}.tm00
EOF5

export pgm=nam_prdgen;
#NCO . prep_step 
ln -sf master${fhr}.ctl                 fort.10
ln -sf $FIXnam/nam_wgt_197         fort.21
ln -sf $PARMnam/nam_kwbx.tbl       fort.41
ln -sf $PARMnam/nam_time.tbl       fort.42
ln -sf $PARMnam/nam_parm.tbl       fort.43
ln -sf $PARMnam/nam_grid.tbl       fort.44
ln -sf $PARMnam/nam_levl.tbl       fort.45

ln -sf input${fhr}.prd             fort.621
$EXECnam/nam_prdgen < input${fhr}.prd > prdgen.out${fhr}
export err=$?;
#NCO err_chk

cp ${rdir}/com/date/t${cyc}z DATE

    mv meso.NDFD mesocon.NDFDf${fhr}
    $utilexec/grbindex mesocon.NDFDf${fhr} mesocon.NDFDif${fhr}

# at 12-hr times, we need to make 12-hr max/min temps
#   we also need 3 and 6-hr buckets

     cp $FIXruc2/ruc2_ndfdtopo.dat ruc2_ndfdtopo.dat
     cp $FIXruc2/ruc2_vegtype_ndfd.dat ruc2_vegtype_ndfd.dat

if [ $fhr -eq 18 -o $fhr -eq 30 -o $fhr -eq 42 -o $fhr -eq 54 -o \
       $fhr -eq 66 -o $fhr -eq 78 ] ; then
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

    export pgm=nam_smartinitcon;
# NCO. prep_step
  ln -sf "mesocon.NDFDf${fhr}"      fort.11
  ln -sf "mesocon.NDFDif${fhr}"     fort.12
  ln -sf "SREFPCP"                  fort.13
  ln -sf "SREFPCPi"                 fort.14
  ln -sf "6precip"            fort.15
  ln -sf "6precipi"           fort.16
  ln -sf "6snow"            fort.17
  ln -sf "6snowi"           fort.18
  ln -sf "12precip"   fort.19
  ln -sf "12precipi"  fort.20
  ln -sf "MAXMIN1"   fort.21
    ln -sf "MAXMIN2"   fort.22
    ln -sf "MAXMIN3"   fort.23
    ln -sf "MAXMIN4"   fort.24
    ln -sf "MAXMIN5"   fort.25
    ln -sf "MAXMIN1i"  fort.26
    ln -sf "MAXMIN2i"  fort.27
    ln -sf "MAXMIN3i"  fort.28
    ln -sf "MAXMIN4i"  fort.29
    ln -sf "MAXMIN5i"  fort.30

    ln -fs "$FIXruc2/ruc2_ndfdtopo.dat" fort.46
    ln -fs "$FIXruc2/ruc2_vegtype_ndfd.dat" fort.47
    $EXECsma/nam_smartinitconus <<EOF >> smartinit.out${fhr}

$fhr
$cyc
EOF
export err=$?;

cp -f MESOCS${fhr}.tm00 $COMOUT/nam.t${cyc}z.smartconus${fhr}.tm00

if [ $NCO -eq 1 ];then
err_chk

#bsm - add processing for conversion to grib2 and awips files
 $utilexec/cnvgrib -g12 -p40 MESOCS${fhr}.tm00 nam.t${cyc}z.smartconus${fhr}.tm00.grib2
# Processing grids for AWIPS
 pgm=tocgrib2
 export pgm;
#NCO. prep_step
 startmsg

 export XLFUNIT_11=nam.t${cyc}z.smartconus${fhr}.tm00.grib2
 export XLFUNIT_31=" "
 export XLFUNIT_51=grib2.t${cyc}z.smartconusf${fhr}

 $utilexec/tocgrib2 <$UTILparm/grib2_awpnamsmartconusofff${fhr}.197 >> $pgmout 2> errfile
 echo " error from tocgrib=",$err

mv MESOCS${fhr}.tm00 $COMOUT/nam.t${cyc}z.smartconus${fhr}.tm00
mv nam.t${cyc}z.smartconus${fhr}.tm00.grib2 $COMOUT/nam.t${cyc}z.smartconus${fhr}.tm00.grib2
mv grib2.t${cyc}z.smartconusf${fhr} $pcom/grib2.awpnamsmart.conus${fhr}_awips_f${fhr}_${cyc}
if [ $SENDDBN = YES ] #bsm 25 feb 2008 - added code for awips alerts
 then
  $DBNROOT/bin/dbn_alert NTC_LOW SMARTCONUS $job $pcom/grib2.awpnamsmart.conus${fhr}_awips_f${fhr}_${cyc}
fi

if [ $SENDDBN_GB2 = YES ]
 then
  $DBNROOT/bin/dbn_alert MODEL NAM_SMARTCONUS_GB2 $job $COMOUT/nam.t${cyc}z.smartconus${fhr}.tm00.grib2
fi

fi

elif [ $check6 -eq 0 -a $fhr -ne 0 ]; then

    cp srefpcpcon_${SREF_PDY}${srefcyc}f0${pcphrl} SREFPCP
    cp srefpcpconi_${SREF_PDY}${srefcyc}f0${pcphrl} SREFPCPi
    cp MAXMIN${fhr2}.tm00 MAXMIN2
    cp MAXMIN${fhr1}.tm00 MAXMIN1

    $utilexec/grbindex MAXMIN2 MAXMIN2i
    $utilexec/grbindex MAXMIN1 MAXMIN1i

    export pgm=nam_smartinitcon;
#NCO . prep_step
    ln -fs "$FIXruc2/ruc2_ndfdtopo.dat" fort.46
    ln -fs "$FIXruc2/ruc2_vegtype_ndfd.dat" fort.47
  ln -sf "mesocon.NDFDf${fhr}"      fort.11
  ln -sf "mesocon.NDFDif${fhr}"     fort.12
  ln -sf "SREFPCP"                  fort.13
  ln -sf "SREFPCPi"                 fort.14
  ln -sf "6precip"            fort.15
  ln -sf "6precipi"           fort.16
  ln -sf "6snow"            fort.17
  ln -sf "6snowi"           fort.18
  ln -sf "MAXMIN2"   fort.19
  ln -sf "MAXMIN1"   fort.20
  ln -sf "MAXMIN2i"  fort.21
  ln -sf "MAXMIN1i"  fort.22

    $EXECsma/nam_smartinitconus <<EOF >> smartinit.out${fhr}
$fhr
$cyc
EOF
export err=$?;
#NCO err_chk

cp MESOCS${fhr}.tm00 $COMOUT/nam.t${cyc}z.smartconus${fhr}.tm00

if [ $NCO -eq 1 ];then
#bsm - add processing for conversion to grib2 and awips files
$utilexec/cnvgrib -g12 -p40 MESOCS${fhr}.tm00 nam.t${cyc}z.smartconus${fhr}.tm00.grib2
# Processing grids for AWIPS
 pgm=tocgrib2
 export pgm;
#NCO . prep_step
 startmsg

 export XLFUNIT_11=nam.t${cyc}z.smartconus${fhr}.tm00.grib2
 export XLFUNIT_31=" "
 export XLFUNIT_51=grib2.t${cyc}z.smartconusf${fhr}

 $utilexec/tocgrib2 <$UTILparm/grib2_awpnamsmartconusofff${fhr}.197 >> $pgmout 2> errfile
 echo " error from tocgrib=",$err

mv MESOCS${fhr}.tm00 $COMOUT/nam.t${cyc}z.smartconus${fhr}.tm00
mv nam.t${cyc}z.smartconus${fhr}.tm00.grib2 $COMOUT/nam.t${cyc}z.smartconus${fhr}.tm00.grib2
mv grib2.t${cyc}z.smartconusf${fhr} $pcom/grib2.awpnamsmart.conus${fhr}_awips_f${fhr}_${cyc}
if [ $SENDDBN = YES ] #bsm 25 feb 2008 - added code for awips alerts
 then
  $DBNROOT/bin/dbn_alert NTC_LOW SMARTCONUS $job $pcom/grib2.awpnamsmart.conus${fhr}_awips_f${fhr}_${cyc}
fi

if [ $SENDDBN_GB2 = YES ]
 then
  $DBNROOT/bin/dbn_alert MODEL NAM_SMARTCONUS_GB2 $job $COMOUT/nam.t${cyc}z.smartconus${fhr}.tm00.grib2
fi
fi

# for all forecast hours divisible by 3,  we need
#    3-hr buckets and max/min temp data for the previous 2 hours

elif [ $check -eq 0 -a $fhr -ne 0 ]; then

    cp srefpcpcon_${SREF_PDY}${srefcyc}f0${pcphrl} SREFPCP
    cp srefpcpconi_${SREF_PDY}${srefcyc}f0${pcphrl} SREFPCPi
    cp MAXMIN${fhr2}.tm00 MAXMIN2
    cp MAXMIN${fhr1}.tm00 MAXMIN1

    $utilexec/grbindex MAXMIN2 MAXMIN2i
    $utilexec/grbindex MAXMIN1 MAXMIN1i

    export pgm=nam_smartinitcon;
#NCO . prep_step
    ln -fs "$FIXruc2/ruc2_ndfdtopo.dat" fort.46
    ln -fs "$FIXruc2/ruc2_vegtype_ndfd.dat" fort.47
  ln -sf "mesocon.NDFDf${fhr}"      fort.11
  ln -sf "mesocon.NDFDif${fhr}"     fort.12
  ln -sf "SREFPCP"                  fort.13
  ln -sf "SREFPCPi"                 fort.14
  ln -sf "MAXMIN2"   fort.15
  ln -sf "MAXMIN1"   fort.16
  ln -sf "MAXMIN2i"  fort.17
  ln -sf "MAXMIN1i"  fort.18
    $EXECsma/nam_smartinitconus <<EOF >> smartinit.out${fhr}
$fhr
$cyc
EOF
export err=$?;
#NCO err_chk

if [ $NCO -eq 1 ];then
#bsm - add processing for conversion to grib2 and awips files
$utilexec/cnvgrib -g12 -p40 MESOCS${fhr}.tm00 nam.t${cyc}z.smartconus${fhr}.tm00.grib2
# Processing grids for AWIPS
 pgm=tocgrib2
 export pgm;
#NCO prep_step
 startmsg

 export XLFUNIT_11=nam.t${cyc}z.smartconus${fhr}.tm00.grib2
 export XLFUNIT_31=" "
 export XLFUNIT_51=grib2.t${cyc}z.smartconusf${fhr}

 $utilexec/tocgrib2 <$UTILparm/grib2_awpnamsmartconusofff${fhr}.197 >> $pgmout 2> errfile
 echo " error from tocgrib=",$err

mv MESOCS${fhr}.tm00 $COMOUT/nam.t${cyc}z.smartconus${fhr}.tm00
mv nam.t${cyc}z.smartconus${fhr}.tm00.grib2 $COMOUT/nam.t${cyc}z.smartconus${fhr}.tm00.grib2
mv grib2.t${cyc}z.smartconusf${fhr} $pcom/grib2.awpnamsmart.conus${fhr}_awips_f${fhr}_${cyc}
if [ $SENDDBN = YES ] #bsm 25 feb 2008 - added code for awips alerts
 then
  $DBNROOT/bin/dbn_alert NTC_LOW SMARTCONUS $job $pcom/grib2.awpnamsmart.conus${fhr}_awips_f${fhr}_${cyc}
fi

if [ $SENDDBN_GB2 = YES ]
 then
  $DBNROOT/bin/dbn_alert MODEL NAM_SMARTCONUS_GB2 $job $COMOUT/nam.t${cyc}z.smartconus${fhr}.tm00.grib2
fi

fi


#  for all "in-between" forecast hours (13,14,16....), we don't need
#    any special data
else 
    export pgm=nam_smartinitcon;
#NCO . prep_step
    ln -fs "$FIXruc2/ruc2_ndfdtopo.dat" fort.46
    ln -fs "$FIXruc2/ruc2_vegtype_ndfd.dat" fort.47
  ln -sf "mesocon.NDFDf${fhr}"      fort.11
  ln -sf "mesocon.NDFDif${fhr}"     fort.12
    $EXECsma/nam_smartinitconus <<EOF >> smartinit.out${fhr}
$fhr
$cyc
EOF

   cp MESOCS${fhr}.tm00 $COMOUT/nam.t${cyc}z.smartconus${fhr}.tm00
if [ $NCO -eq 1 ];then
#NCO Commands follow
export err=$?;
#NCO err_chk

if [ $fhr -eq 0 ] ; then
   #bsm - add processing for conversion to grib2 and awips files
   $utilexec/cnvgrib -g12 -p40 MESOCS${fhr}.tm00 nam.t${cyc}z.smartconus${fhr}.tm00.grib2
   # Processing grids for AWIPS
   pgm=tocgrib2
   export pgm;
#NCO . prep_step
   startmsg

   export XLFUNIT_11=nam.t${cyc}z.smartconus${fhr}.tm00.grib2
   export XLFUNIT_31=" "
   export XLFUNIT_51=grib2.t${cyc}z.smartconusf${fhr}

   $utilexec/tocgrib2 <$UTILparm/grib2_awpnamsmartconusonf${fhr}.197 >> $pgmout 2> errfile
   echo " error from tocgrib=",$err

   mv MESOCS${fhr}.tm00 $COMOUT/nam.t${cyc}z.smartconus${fhr}.tm00
   mv nam.t${cyc}z.smartconus${fhr}.tm00.grib2 $COMOUT/nam.t${cyc}z.smartconus${fhr}.tm00.grib2
   mv grib2.t${cyc}z.smartconusf${fhr} $pcom/grib2.awpnamsmart.conus${fhr}_awips_f${fhr}_${cyc}
   if [ $SENDDBN = YES ] #bsm 25 feb 2008 - added code for awips alerts
    then
     $DBNROOT/bin/dbn_alert NTC_LOW SMARTCONUS $job $pcom/grib2.awpnamsmart.conus${fhr}_awips_f${fhr}_${cyc}
   fi

   if [ $SENDDBN_GB2 = YES ]
    then
     $DBNROOT/bin/dbn_alert MODEL NAM_SMARTCONUS_GB2 $job $COMOUT/nam.t${cyc}z.smartconus${fhr}.tm00.grib2
   fi
  fi

fi #NCO

fi
done
exit
