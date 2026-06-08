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
case $cyc in 
  00|12) cyctp=on;;
  06|18) cyctp=off;;
esac

REGCP=`echo $outreg |tr '[a-z]'  '[A-Z]' `
echo BEGIN NCO sminit Post-Processing for REG $RGIN $outreg $ogrd CYC $cyc FHR $fhr 

# Change grid id number from 188 to 255
if [ $outreg = conus2p5 ];then
  cp MESO${RGIN}${fhr}.tm00 $COMOUT/${mdl}.t${cyc}z.smart${outreg}${fhr}.tm00_wexp.grib2
fi

if [ $outreg != conus2p5 ];then
  # Make u/v part of one record [AMG - Aug 2016]
  cp MESO${RGIN}${fhr}.tm00 MESO${RGIN}${fhr}.tm00.uv
  $WGRIB2 MESO${RGIN}${fhr}.tm00.uv -new_grid_vectors "UGRD:VGRD" -submsg_uv MESO${RGIN}${fhr}.tm00
  # End make u/v part of one record
  mv MESO${RGIN}${fhr}.tm00 $COMOUT/${mdl}.t${cyc}z.smart${outreg}${fhr}.tm00.grib2
fi

rm *grb2* MESO*

# Mercator PR 1.25 grid
if [ $outreg = pr ];then
  ogrd=1p25
fi

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
  awpparm=$PARMdng/wmo/grib2_awips_${mdl}_smartconus_${cyctp}_f0${fhr}
elif [ $outreg = ak3 ];then
  awpparm=$PARMdng/wmo/grib2_awips_${mdl}_smartak_${cyctp}_f0${fhr}
else
  awpparm=$PARMdng/wmo/grib2_awips_${mdl}_smart${outreg}_${cyctp}_f0${fhr}
fi

if [ -s "$awpparm" ];then
  $TOCGRIB2 < ${awpparm} 1 >> $pgmout 2>> errfile
  echo " error from tocgrib="  $err
else 
  echo AWP PARM FILE not found: $awpparm
fi

fi # awpchk -eq 0

# Move grib2 awips file to $COMOUTwmo
if [ $awpchk -eq 0 ];then
if [ $outreg = ak3 ];then
  mv grib2.t${cyc}z.smart${outreg}f${fhr} ${COMOUTwmo}/grib2.awp${mdl}smart3.ak${fhr}_awips_f${fhr}_${cyc}
elif [ $outreg = pr ];then
  mv grib2.t${cyc}z.smart${outreg}f${fhr} ${COMOUTwmo}/grib2.awp${mdl}smart1p25.${outreg}${fhr}_awips_f${fhr}_${cyc}
else
  mv grib2.t${cyc}z.smart${outreg}f${fhr} ${COMOUTwmo}/grib2.awp${mdl}smart.${outreg}${fhr}_awips_f${fhr}_${cyc}
fi

if [ -s "$awpparm" ];then
  if [ "$SENDDBN" = YES ];then #bsm 25 feb 2008 - added code for awips alerts
    if [ $outreg = ak3 ];then
      $SIPHONROOT/bin/dbn_alert NTC_LOW SMART${REGCP} $job ${COMOUTwmo}/grib2.awp${mdl}smart3.ak${fhr}_awips_f${fhr}_${cyc}
    elif [ $outreg = pr ];then
      $SIPHONROOT/bin/dbn_alert NTC_LOW SMART${REGCP} $job ${COMOUTwmo}/grib2.awp${mdl}smart1p25.${outreg}${fhr}_awips_f${fhr}_${cyc}
    else
      $SIPHONROOT/bin/dbn_alert NTC_LOW SMART${REGCP} $job ${COMOUTwmo}/grib2.awp${mdl}smart.${outreg}${fhr}_awips_f${fhr}_${cyc}
    fi
  fi
fi

fi # awpchk -eq 0

if [ "$SENDDBN" = YES ];then
  $SIPHONROOT/bin/dbn_alert MODEL RRFS_SMART${REGCP}_GB2 $job $COMOUT/${mdl}.t${cyc}z.smart${outreg}${fhr}.tm00.grib2
fi
