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

set -x
#msg="JOB $job HAS BEGUN"
#NCO postmsg "$jlogfile" "$msg"

cd $DATA
cp ${PARMdng}/SMINIT.CTL .

export fhrstr=00

echo `date +%T` "Submit smartinit" $RUNTYP  CYC=$cyc  FHR=$ffhr 
$USHdng/smartinit_g2.sh

#####################################################################

