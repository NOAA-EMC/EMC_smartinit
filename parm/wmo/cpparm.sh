#!/bin/ksh

typeset -Z2 fhr
mdl=nam
endhrs=84

#for rgn in pr conus;do
for rgn in pr;do
  case $rgn in 
      pr)grd=1p25; ogrd=195;;
#     pr)grd=254; ogrd=195;;
#     conus)grd=188; ogrd=184;;
  esac
  for cctp in on off;do
    fhr=00
    while [ $fhr -le $endhrs ];do
      ffold=grib2_awp${mdl}dng${rgn}${cctp}f${fhr}.${ogrd}
      ffnew=grib2_awp${mdl}dng${rgn}${cctp}f${fhr}.${grd}
      echo $ffold
      echo $ffnew
      cp $ffold $ffnew
      ((fhr=fhr+3))
    done
  done
done
