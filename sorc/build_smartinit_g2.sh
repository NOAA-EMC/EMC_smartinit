BASE=`pwd`
export BASE

 . $MODULESHOME/init/bash

#. /usrx/local/Modules/default/init/ksh
 . /usrx/local/prod/lmod/lmod/init/profile
module purge
module load EnvVars/1.0.2
moduledir=`dirname $(readlink -f ../modulefiles/SMARTINIT)`
module use ${moduledir}
module load SMARTINIT/v4.2.1
#module use $BASE/../modulefiles/SMARTINIT
#module load $BASE/../modulefiles/SMARTINIT/v4.2.0
module list

sleep 1

##############################

cd ${BASE}/smartinit_g2.fd
make clean
make 
make clean

##############################
