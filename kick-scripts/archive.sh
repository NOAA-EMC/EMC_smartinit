#!/bin/ksh
# Archive para smartinit files

export yyyymmdd=`/nwprod/util/exec/ndate -24 |cut -c 1-8`
# export yyyymmdd=20131117
export wdir=/stmp/${USER}/archive
indir=/ptmp/${USER}/nam.$yyyymmdd

mkdir -p $wdir
cd $wdir

 for REG in conus conus2p5 ak ak3 hi pr;do
  tar -cvf smart${REG}.${yyyymmdd}.tar  ${indir}/nam.t??z.smart${REG}??.tm00
  hsi put smart${REG}.${yyyymmdd}.tar /NCEPDEV/hpssuser/g01/wx22mc/smartpara/smart${REG}.${yyyymmdd}.tar
 done
