#!/bin/bash
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         smartinit_subpiece.sh
# Script description:  Run parallel (cfp) wgrib2 interpolation script
#
# Author:        Annette Gibbs   Org: NOAA/EMC         Date: 2025-11-16
#
# Abstract: This script does the parallel wgrib2 interpolation script
# Adapted from the rrfs_prdgen_subpiece.sh script
#
# Script history log:
# 2025-11-16  Annette Gibbs
#

set -x

subpiece=$1
DATA=$2
infile=$3
wgrib2def="$4"
compress="$5" 
interp="$6"

cd $DATA
parmfile=$DATA/inventory.txt${subpiece}

# Use different parm file for each subpiece

wgrib2 ${infile} | grep -F -f ${parmfile} | wgrib2 -i -grib inputs.grb2_${subpiece} ${infile}
wgrib2 inputs.grb2_${subpiece} -set_grib_type ${compress} -new_grid_winds grid ${interp} \
  -if ":(SFCR|VGTYP|LAND):" -new_grid_interpolation neighbor -fi \
  -new_grid ${wgrib2def} model.ndfd_${subpiece}

# reassemble data in the smartinit_g2.sh script

