#!/bin/ksh

#BSUB -o /u/Annette.Gibbs/logs/put_smartnam_hpss.out.%J
#BSUB -e /u/Annette.Gibbs/logs/put_smartnam_hpss.out.%J
#BSUB -q "transfer"
#BSUB -R "rusage[mem=100]"
#BSUB -R "affinity[core(1)]"
#BSUB -W "1:30"
#BSUB -P "RTMA-T2O"
#BSUB -J "smartNAMpush"

#========================================
# Archive para smartinit files
#========================================

. /usrx/local/Modules/3.2.9/init/ksh

set -x

PDY=20150729
which_nam=para
code_ver=v3.4.0

echo $PDY
YYYY=`echo $PDY | cut -c1-4`
YYYYMM=`echo $PDY | cut -c1-6`
YYYYMMDD=`echo $PDY | cut -c1-8`

loc_path=/meso2/noscrub/Annette.Gibbs/${code_ver}/nam.${YYYYMMDD}

hpsspath=/2year/NCEPDEV/emc-meso/Annette.Gibbs/${YYYY}/${YYYYMM}/${YYYYMMDD}

domain="conus ak pr hi"
hrs="00 06 12 18"

cd $loc_path

hsi "mkdir -p ${hpsspath}"

for reg in ${domain}; do

  for fhrs in ${hrs}; do

  hpssfile=${which_nam}_nam.${YYYYMMDD}${fhrs}.smart${reg}
  htar -cvf ${hpsspath}/${hpssfile}.tar nam.t${fhrs}z.smart${reg}*

  done

done

echo "Done transferring files to HPSS"

exit

