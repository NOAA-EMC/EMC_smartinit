#!/bin/ksh
#========================================
# Archive para smartinit files
#========================================

. /usrx/local/Modules/3.2.9/init/ksh
module load ibmpe ics lsf

export yyyymmdd=`/nwprod/util/exec/ndate -24 |cut -c 1-8`
#TEST export yyyymmdd=20140307   #TEST

# Location of smartinit gif plot files
#===========================================================
export pldir=/stmpp1/${USER}/smartplt
#===========================================================

hpssdir=/NCEPDEV/hpssuser/g01/wx22mc

export wdir=${pldir}/archive

mkdir -p $wdir

for mdl in nam dgex;do
# Location of smartinit grib files
#==========================================
  indir=/ptmpp1/${USER}/${mdl}.${yyyymmdd}
#==========================================
  case $mdl in 
    nam)
      export regions="conus conus2p5 ak ak3 hi pr"
      export plregs="${regions} ase boi mfr nyc pajn phnl sdb vgt pu hi";;
    dgex)
      export regions="conus ak3" 
      export plregs="conus2p5 ak3";;
  esac

# Archive GRIB files
  for REG in ${regions};do
    cd $wdir
    if [ -s $indir ];then
     /gpfs/gd2/emc/hwrf/noscrub/soft/emc-utils-1.0.0/bin/hpsstar \
     put ${hpssdir}/smartpara/${mdl}smart${REG}.${yyyymmdd}.tar \
                  ${indir}/${mdl}.t??z.smart${REG}??.tm00
    else
      echo MODEL DIR  ${indir} $REG  NOT FOUND
    fi
  done

# Archive Plot files
  for REG in ${plregs};do
    ifound=0
    rm -rf $pldir/plarchive
    mkdir -p $pldir/plarchive
    for cyc in 00 06 12 18;do
      if [ -s $pldir/d2${mdl}${REG}$cyc ];then
        cd $pldir/d2${mdl}${REG}${cyc}
        cp *gif $pldir/plarchive
        ifound=1
      else
        echo $mdl $cyc $REG PLOT DIR  ${pldir}/d2${mdl}${REG}${cyc}  NOT FOUND
      fi
    done
    cd $pldir/plarchive
    if [ $ifound = 1 ];then
      hpsstar put ${hpssdir}/smartplts/${mdl}smartplts${REG}.${yyyymmdd}.tar *gif;fi
  done
done  #MDL Loop
