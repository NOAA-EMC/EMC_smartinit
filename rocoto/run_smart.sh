#!/bin/bash

set -x

export exp=""

module purge
module load envvar/1.0
module load PrgEnv-intel/8.1.0
module load craype/2.7.8
module load intel/19.1.3.304
module load cray-mpich/8.1.7
module load cray-pals/1.0.12
module load prod_util/2.0.9
module load prod_envir/2.0.4
module load libjpeg/9c
module load grib_util/1.2.2
module load wgrib2/2.0.8_wmo
module load cfp/2.0.4

module use /apps/ops/test/nco/modulefiles
module load core/rocoto/1.3.5

rocotorun -v 10 -w /lfs/h2/emc/da/save/${USER}/packages/RB-v5.0.0${exp}/rocoto/drive_smart.xml -d /lfs/h2/emc/da/save/${USER}/packages/RB-v5.0.0${exp}/rocoto/drive_smart.db
