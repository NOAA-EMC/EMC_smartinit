set -x

BASE=`pwd`
export BASE

cd $BASE

mkdir $BASE/logs
export logs_dir=$BASE/logs

. /usrx/local/Modules/default/init/ksh
module purge
module load $BASE/../modulefiles/SMARTINIT/v4.2.0
module list

sleep 1

##############################

echo " .... Building smartinit_g2 .... "
./build_smartinit_g2.sh > $logs_dir/build_smartinit_g2.log 2>&1
cd $BASE

##############################

echo " .... Building smartprecip_g2 .... "
./build_smartprecip_g2.sh > $logs_dir/build_smartprecip_g2.log 2>&1
cd $BASE

##############################
