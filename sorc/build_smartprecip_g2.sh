set -x

BASE=`pwd`
export BASE

 . /usrx/local/prod/lmod/lmod/init/profile
module purge
module load EnvVars/1.0.2
moduledir=`dirname $(readlink -f ../modulefiles/SMARTINIT)`
module use ${moduledir}
module load SMARTINIT/v4.3.0
module list

sleep 1

##############################

cd ${BASE}/smartprecip_g2.fd
make clean
make 
make clean

##############################
