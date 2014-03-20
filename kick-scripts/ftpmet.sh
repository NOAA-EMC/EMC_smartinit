#!/bin/ksh
#========================================
# FTP smartinit haines index files
#========================================
. /usrx/local/Modules/3.2.9/init/ksh
module load ibmpe ics lsf

CYC=${CYC:-${1}}
strhr=${strhr:-00}
endhr=${endhr:-12}
freq=${freq:-03}

export yyyymmdd=`/nwprod/util/exec/ndate |cut -c 1-8`
export pldir=/stmpp1/${USER}/smartplt
export wdir=${pldir}/ftpmet
export rzdmdir=/home/people/emc/ftp/mmb/aq/haines
export regions="conus2p5 ak3 hi pr"
export VARB=hindex

mkdir -p $wdir
cd $wdir

#TEST for mdl in nam dgex;do
for mdl in nam ;do
  indir=/ptmpp1/${USER}/${mdl}.${yyyymmdd} 
  for REG in ${regions};do
    rm -f temp
    fhr=$strhr
    while [ $fhr -le $endhr ];do
      cat ${indir}/${VARB}.t${CYC}z.smart${REG}${fhr}.tm00.grib2 >>temp
      ((fhr=fhr+freq))
      if [ $fhr -lt 10 ];then fhr=0${fhr};fi
    done
    mv temp ${indir}/${VARB}.t${CYC}z.smart${REG}.tm00.grib2
    scp -p ${indir}/${VARB}.t${CYC}z.smart${REG}.tm00.grib2  wd22jm@emcrzdm:${rzdmdir}
  done
done

