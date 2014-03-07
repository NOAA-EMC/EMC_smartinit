#!/bin/ksh
#========================================
# FTP smartinit haines index files
#========================================
. /usrx/local/Modules/3.2.9/init/ksh
module load ibmpe ics lsf

CYC=${CYC:-00}
export yyyymmdd=`/nwprod/util/exec/ndate |cut -c 1-8`
export pldir=/stmpp1/${USER}/smartplt
export wdir=${pldir}/ftpmet
export rzdmdir=/home/people/emc/ftp/mmb/aq/haines
export regions="conus2p5 ak3 hi pr"
export VARB=hindex

mkdir -p $wdir

for mdl in nam dgex;do
  indir=/ptmpp1/${USER}/${mdl}.${yyyymmdd} 
  for REG in ${regions};do
    cd $wdir
    tar -cvf ${VARB}smart${REG}.${yyyymmdd}.tar  ${indir}/${VARB}.t${CYC}z.smart${REG}??.tm00.grib2
    scp -p ${VARB}smart${REG}.${yyyymmdd}.tar  wd22jm@${rzdmdir}
  done
done

