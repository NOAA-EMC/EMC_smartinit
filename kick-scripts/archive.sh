#!/bin/ksh
# Archive para smartinit files

export yyyymmdd=`/nwprod/util/exec/ndate -24 |cut -c 1-8`
# export yyyymmdd=20131117
export wdir=/stmp/${USER}/archive
export pldir=/stmp/${USER}
indir=/ptmp/${USER}/nam.$yyyymmdd

mkdir -p $wdir

 for REG in conus conus2p5 ak ak3 hi pr;do
# Archive GRIB files
  cd $wdir
  tar -cvf smart${REG}.${yyyymmdd}.tar  ${indir}/nam.t??z.smart${REG}??.tm00
  hsi put smart${REG}.${yyyymmdd}.tar /NCEPDEV/hpssuser/g01/wx22mc/smartpara/smart${REG}.${yyyymmdd}.tar

# Archive Plot files
  for cyc in 00 06 12 18;do
    cd $pldir/d2${REG}${cyc}
    if [ -s ../smartplts${REG}.${yyyymmdd}.tar ];then
      tar --append --file=../smartplts${REG}.${yyyymmdd}.tar  *gif
    else
      tar -cvf ../smartplts${REG}.${yyyymmdd}.tar  *gif
    fi
  done
  hsi put ../smartplts${REG}.${yyyymmdd}.tar /NCEPDEV/hpssuser/g01/wx22mc/smartplts/smartplts${REG}.${yyyymmdd}.tar
 done
