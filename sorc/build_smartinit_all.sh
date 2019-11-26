set -x

BASE=`pwd`
export BASE

cd $BASE

mkdir $BASE/logs
export logs_dir=$BASE/logs

#. /usrx/local/Modules/default/init/ksh
 . /usrx/local/prod/lmod/lmod/init/profile
module purge
moduledir=`dirname $(readlink -f ../modulefiles/SMARTINIT)`
module use ${moduledir}
module load SMARTINIT/v4.3.0
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
