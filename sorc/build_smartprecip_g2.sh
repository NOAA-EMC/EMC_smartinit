set -x

BASE=`pwd`
export BASE

# . /usrx/local/prod/lmod/lmod/init/profile
source /apps/prod/lmodules/startLmod
module purge
module load envvar/1.0
moduledir=`dirname $(readlink -f ../modulefiles/SMARTINIT)`
module use ${moduledir}
#module load SMARTINIT/v4.3.2
source ${moduledir}/SMARTINIT/v4.3.2
module list

sleep 1

##############################

cd ${BASE}/smartprecip_g2.fd
make clean
make 
make clean

##############################
