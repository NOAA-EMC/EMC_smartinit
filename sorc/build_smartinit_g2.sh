BASE=`pwd`
export BASE

. /usrx/local/Modules/default/init/ksh
module purge
module load $BASE/../modulefiles/SMARTINIT/v4.2.0
module list

sleep 1

##############################

cd ${BASE}/smartinit_g2.fd
make clean
make 
make clean

##############################
