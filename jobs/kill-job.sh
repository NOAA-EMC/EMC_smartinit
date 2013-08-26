#!/bin/ksh
# 
for jname in DRIVER RUNALL;do
  ps -ef -U $USER |grep $jname
  jobid=`ps -ef  -u $USER |grep $jname | awk '{ print $2 }'`
  kill -9 $jobid
done
