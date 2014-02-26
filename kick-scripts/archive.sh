#!/bin/ksh
#========================================
# Archive para smartinit files
#========================================

. /usrx/local/Modules/3.2.9/init/ksh
module load ibmpe ics lsf

export yyyymmdd=`/nwprod/util/exec/ndate -24 |cut -c 1-8`
export pldir=/stmpp1/${USER}/smartplt
export wdir=${pldir}/archive
export regions="conus conus2p5 ak ak3 hi pr"
export plregs="conus conus2p5 ak ak3 hi pr"

mkdir -p $wdir

for mdl in nam dgex;do
  indir=/ptmpp1/${USER}/${mdl}.$yyyymmdd
  if [ $mdl = nam ];then 
    export plregs="${plegs} ase boi mfr nyc pajn phnl sdb vgt pu hi"
  fi

# Archive GRIB files
  for REG in ${regions};do
    cd $wdir
    tar -cvf smart${REG}.${yyyymmdd}.tar  ${indir}/${mdl}.t??z.smart${REG}??.tm00
    hsi put smart${REG}.${yyyymmdd}.tar /NCEPDEV/hpssuser/g01/wx22mc/smartpara/smart${REG}.${yyyymmdd}.tar
  done

# Archive Plot files
  for REG in ${plregs};do
    for cyc in 00 06 12 18;do
      if [ -s $pldir/d2${REG}$cyc ];then
        cd $pldir/d2${REG}${cyc}
        if [ -s ${wdir}/smartplts${REG}.${yyyymmdd}.tar ];then
          tar --append --file=${wdir}/smartplts${REG}.${yyyymmdd}.tar  *gif
        else
          tar -cvf ${wdir}/smartplts${REG}.${yyyymmdd}.tar  *gif
        fi
      else
        echo PLOT DIR to be ARCHIVED ${pldir}/d2${REG}${cyc}  NOT FOUND
      fi
    done
    hsi put ${wdir}/smartplts${REG}.${yyyymmdd}.tar \
           /NCEPDEV/hpssuser/g01/wx22mc/smartplts/smartplts${REG}.${yyyymmdd}.tar
  done
done
