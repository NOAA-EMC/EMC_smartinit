#!/bin/ksh

# move parm files to new filename convention

typeset -Z2 fhr
mdl=nam
endhrs=60

for rgn in conus ak;do
  case $rgn in 
     conus)grd=197;;
        ak)grd=198;;
  esac
  for cctp in on off;do
    fhr=06
    while [ $fhr -le $endhrs ];do
      ffold=grib2_awp${mdl}smart${rgn}${cctp}f${fhr}.${grd}
      ffnew=grib2_awp${mdl}dng${rgn}${cctp}f${fhr}.${grd}
      echo $ffold
      echo $ffnew
      svn delete  --force $ffnew
      svn mv $ffold $ffnew
      ((fhr=fhr+3))
    done
  done
done
