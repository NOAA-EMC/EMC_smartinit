#!/bin/ksh
#=======================================================================
# Script developed by NCO to :
# bsm - add processing for conversion to grib2 and awips files
# called by : smartinit.sh
# INPUT:
#  RGIN :  2 letter Region id (eg: CS, HI, PR,AK..)
#  cyc  :  UTC run cycle
#  fhr  :  Forecasts hour
#  ogrd :  output NDFD grid number (eg: 197,196,195,198...)
#  outreg  :  output file region name (eg: conus,ak,pr,hi,conus2p5,ak3
#=======================================================================
set -x

outreg=$1    # mdlgrd used for guam to distinguish arw/nmm
awpchk=$2    # only create AWIPS files every 3 hours; awpchk=0
ihindex=0
case $cyc in 
  00|12) cyctp=on;;
  06|18) cyctp=off;;
esac
if [ $mdl = dgex ];then cyctp=;fi

REGCP=`echo $outreg |tr '[a-z]'  '[A-Z]' `
echo BEGIN NCO sminit Post-Processing for REG $RGIN $outreg $ogrd CYC $cyc FHR $fhr 

if [ $RGIN = AKRT ];then
  outreg=ak_rtmages
fi

# Change grid id number from 188 to 255
if [ $outreg = conus2p5 ];then
 ogrd=184
 pgm=smartinit_overgridnum_grib
 export pgm;  . prep_step
 echo 255 > input
 rm fort.11 fort.51
 mv MESO${RGIN}${fhr}.tm00 MESO${RGIN}${fhr}.tm00.grb188
 ln -s  MESO${RGIN}${fhr}.tm00.grb188 fort.11
 ln -s MESO${RGIN}${fhr}.tm00.grb255 fort.51
 $EXECdng/smartinit_overgridnum_grib < input > overgridnum_grib.out${fhr}
 if [ $fhr -le 12 ];then
   cp  MESO${RGIN}${fhr}.tm00.grb255  $COMOUT/${mdl}.t${cyc}z.smart${outreg}${fhr}.tm00_wexp
 fi
 cp MESO${RGIN}${fhr}.tm00.grb188 $COMOUT/${mdl}.t${cyc}z.smart${outreg}${fhr}.tm00_grb188
 if [ $fhr -eq 60 ];then
   rm $COMOUT/${mdl}.t${cyc}z.smart*_grb188
 fi
#mv  MESO${RGIN}${fhr}.tm00.grb255  MESO${RGIN}${fhr}.tm00
 $COPYGB -g 184 -x MESO${RGIN}${fhr}.tm00.grb255 MESO${RGIN}${fhr}.tm00 
fi

#$utilexec/cnvgrib -g12 -p40 MESO${RGIN}${fhr}.tm00 ${mdl}.t${cyc}z.smart${outreg}${fhr}.tm00.grib2
$CNVGRIB -g12 -p40 MESO${RGIN}${fhr}.tm00 ${mdl}.t${cyc}z.smart${outreg}${fhr}.tm00.grib2
cp ${mdl}.t${cyc}z.smart${outreg}${fhr}.tm00.grib2 old.grb2

# Correct for grib2 precision (AMG)
# This needs to be done, because when terrain and land/water mask was converted from grib2 to grib1, precision was lost
# in some of the grid specs.
# Will need to add other domains, once new EMC/GFE common terrain and land/water mask fields are used
# Rethink if you need to do this here.  
# Perhaps you can do this directly within the Terrain and Land/Sea mask files (/meso/save/Annette.Gibbs/bin2grib)
# Also sorc/smartinit.fd/w3fi71.f, ush/smartinit.sh, and parm/SMINIT.CTL

#if [ $outreg = pr ];then
# Mercator PR 1.25 grid
# $WGRIB2 -set_int 3 39 16977485 old.grb2 -grib new.grb2_1
# $WGRIB2 -set_int 3 43 291972167 new.grb2_1 -grib new.grb2_2
# $WGRIB2 -set_int 3 56 296015600 new.grb2_2 -grib ${mdl}.t${cyc}z.smart${outreg}${fhr}.tm00.grib2
#elif [ $outreg = conus2p5 ];then

#if [ $outreg = conus2p5 ];then
# Expanded CONUS Nest
# $WGRIB2 -set_int 3 39 19228976 old.grb2 -grib new.grb2_1
# $WGRIB2 -set_int 3 43 233723448 new.grb2_1 -grib new.grb2_2
# $WGRIB2 -set_int 3 56 2539703.000 new.grb2_2 -grib new.grb2_3
# $WGRIB2 -set_int 3 60 2539703.000 new.grb2_3 -grib ${mdl}.t${cyc}z.smart${outreg}${fhr}.tm00.grib2
#fi

rm *grb2*
# End Correct for grib2 precision (AMG)

if [ $RGIN != AKRT ];then

if [ $awpchk -eq 0 ];then

# Processing grids for AWIPS
 pgm=tocgrib2
 export pgm;  . prep_step
 startmsg
export FORTREPORTS=unit_vars=yes
export FORT11=${mdl}.t${cyc}z.smart${outreg}${fhr}.tm00.grib2 
export FORT31="";
export FORT51=grib2.t${cyc}z.smart${outreg}f${fhr}

# Define grib2 awips parm file 
if [ $outreg = conus2p5 ];then
  awpparm=$utilparm/grib2_awp${mdl}dngconus${cyctp}f${fhr}.${ogrd}
elif [ $outreg = ak3 ];then
  awpparm=$utilparm/grib2_awp${mdl}dngak${cyctp}f${fhr}.${ogrd}
elif [ $outreg = guam ];then
  awpparm=$UTILdng/parm/grib2_${mdl}_smart${outreg}${cyctp}f${fhr}.${ogrd}
else
  awpparm=$utilparm/grib2_awp${mdl}dng${outreg}${cyctp}f${fhr}.${ogrd}
fi

if [ -s "$awpparm" ];then
  $TOCGRIB2 < ${awpparm} 1 >> $pgmout 2>> errfile
  echo " error from tocgrib="  $err
else 
  echo AWP PARM FILE not found: $awpparm
fi

fi # awpchk -eq 0

fi # RGIN != AKRT

mv MESO${RGIN}${fhr}.tm00 $COMOUT/${mdl}.t${cyc}z.smart${outreg}${fhr}.tm00
mv ${mdl}.t${cyc}z.smart${outreg}${fhr}.tm00.grib2 $COMOUT/${mdl}.t${cyc}z.smart${outreg}${fhr}.tm00.grib2
if [ $ihindex -eq 1 ];then
 mv hindex.t${cyc}z.smart${outreg}${fhr}.tm00 $COMOUT/hindex.t${cyc}z.smart${outreg}${fhr}.tm00
 mv hindex.t${cyc}z.smart${outreg}${fhr}.tm00.grib2 $COMOUT/hindex.t${cyc}z.smart${outreg}${fhr}.tm00.grib2
fi

# Move grib2 awips file to pcom
if [ $RGIN != AKRT ];then
if [ $awpchk -eq 0 ];then
if [ $outreg = ak3 ];then
  mv grib2.t${cyc}z.smart${outreg}f${fhr} $pcom/grib2.awp${mdl}smart3.ak${fhr}_awips_f${fhr}_${cyc}
else
  mv grib2.t${cyc}z.smart${outreg}f${fhr} $pcom/grib2.awp${mdl}smart.${outreg}${fhr}_awips_f${fhr}_${cyc}
fi

if [ -s "$awpparm" ];then
  if [ $SENDDBN = YES ];then #bsm 25 feb 2008 - added code for awips alerts
    if [ $outreg = ak3 ];then
      $DBNROOT/bin/dbn_alert NTC_LOW SMART${REGCP} $job $pcom/grib2.awp${mdl}smart3.ak${fhr}_awips_f${fhr}_${cyc}
    else
      $DBNROOT/bin/dbn_alert NTC_LOW SMART${REGCP} $job $pcom/grib2.awp${mdl}smart.${outreg}${fhr}_awips_f${fhr}_${cyc}
    fi
  fi
fi

fi # awpchk -eq 0
fi # RGIN != AKRT

if [ $SENDDBN_GB2 = YES ];then
  $DBNROOT/bin/dbn_alert MODEL NAM_SMART${REGCP}_GB2_PARA $job $COMOUT/${mdl}.t${cyc}z.smart${outreg}${fhr}.tm00.grib2
fi
