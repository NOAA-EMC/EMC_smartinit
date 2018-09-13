#!/bin/ksh
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exnam_smartinit.sh
# Script description:  Run NAM smartiniti downscaling jobs
#
# Author:        Bradley Mabe       Org: NP11         Date: 2007-08-23
#
# Abstract: This script runs the Nam  Smart Init jobs
#
# Script history log:
# 2007-08-23  Bradley Mabe  - Created script (copied actually) to call nam_smartinit on & off
#                          initially of Alaskan precip & snowfall
# 2007-09-17  Geoff Manikin - This version for CONUS runs
# 2011-07-06  Geoff Manikin - Adapted to use nested output
# 2011-07-22  Geoff Manikin - Can't use nest output for F00 
# 2012-12-22  Jeff McQueen  - Modified for unified smartinit system
#                             where configuration is controlled by RUNTYP Variable
# 2013-07-01  Jeff McQueen  - Enabled hrw guamnest smartinit ndfd grid 199 generation 
# 2013-07-02  Jeff McQueen  - Modified to create all 00 hour downscaling from NAM parent
#                             While 03 --> 54/60 created from NAM Nests
# 2013-08-27  JTM           - Put in vertical structure
#
# NCO SUBMISSION  HRS       RUNTYP
#                 00-12     ak_rtmages
#                 00-84     conus,ak,hi,pr
#                 00-60     aknest3,conusnest2p5

# NOTE: NCO should not run jobs for RUNTYP=ak from 12 - 54/60 hours
#

#NCO set -xa
msg="JOB $job HAS BEGUN"
#NCO postmsg "$jlogfile" "$msg"

cd $DATA

# 00/12 UTC CYCLE smartinit CONFIGURE
  export ENDHR=60
  export fhrstr=00

# For Off-Cycles must stop nest at 54 hours, so that 12-hr totals at
# f66 aren't split between nest and parent
  if [ cyc -eq 06 -o cyc -eq 18 ]; then export ENDHR=54;fi
  case $RUNTYP in
              ak_rtmages) export ENDHR=12;;
                 gm|guam) export ENDHR=48;;       
    conusnest2p5|aknest3) export ENDHR=60;;
  esac

# For 5 km NAM downscaled NDFD output grids:
#   Downscale from NAM nest for forecast hrs 03-->54/60
#   Downscale from MDL NAM parent for forecast hrs=0 and > ENDHR (54/60)
 
  case $RUNTYP in gm|guam) export RUNTYP=guamnest;; esac

# Need to use the NAM parent because NAM nest does not go out to 84 hours

  if [ $ffhr -gt $ENDHR ];then
    case $RUNTYP in
         hawaiinest) export RUNTYP=hi;;
         priconest) export RUNTYP=pr;;
      esac
  fi

  echo `date +%T` "Submit smartinit" $RUNTYP  CYC=$cyc  FHR=$ffhr 
${SMINIT_SSH:-$USHdng/smartinit_g2.sh}

#####################################################################

echo EXITING $0
exit
#
