set -x
BASE=`pwd`
export BASE

module reset
moduledir=`dirname $(readlink -f ../modulefiles/SMARTINIT)`
module use ${moduledir}
source ../versions/build.ver
module load SMARTINIT/4.4.0.lua
module list

sleep 1

##############################

cd ${BASE}/smartinit_g2.fd
make clean
make 
make clean

##############################
