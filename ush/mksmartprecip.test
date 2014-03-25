#!/bin/ksh 
#
#======================================================================
#  mksmartprecip.sh
#  Creates special 3,6 or 12 hour precip files for smartinit input
#
# Script history log:
# 2012-10-22  JTM : Combined addprecip and makeprecip codes
# 2013-11-20  JTM : Added option to downscale DGEX 3 hrly files beyond 84 hrs w/ 6 hr precip
# 2014-02-25  JTM : Extracted from smartinit.sh 
#======================================================================

#-------------------------------------------------------------
#   NAM OFF-CYC,hiresw & Nests: Create 6/12 hour buckets, 3 hr buckets available
#   NAM ON-CYC :
#     3hr precip available at only 3,15, 27,39... forcast fhours
#     other hours, create 3 hr precip
#     6hr precip: Create only at 00/12 UTC valid times 
#           eg: fhr=12,24,36
#     Create 12 hour precip at 00/12 UTC valid times
#
#   DGEX: create 3 hr buckets at 6 hr times (check6=0)
#         create 6 hr buckets at 3 hr times (check=0, check6.ne.0)
#-------------------------------------------------------------

#===================================================================================
# DETERMINE IF SPECIAL 3, 6 or 12 hour PRECIP FILE SHOULD BE MADE
#===================================================================================
    mk3p=0
    if [ $check6 -eq 0 -a $rg != dgx ];then 
      mk6p=6
      ppgm=add
    fi
#   hr3bkt flag determines when to run smartprecip to create 3 hr buckets
    let hr3bkt=$((fhr-3))%12

#-------------------------------------------------------------
#   ON-CYC: The 3-hr fhrs (3,15,27,39....) --> Already have 3-hr buckets
#   For in-between fhrs (22,23,25) --> Create 3-hr buckets
#   since we only gather max/min data at those hours to compute 12 hr max/mins
#   DGEX has 6 hr precip at 6hr times and 3 hr precip at other times
#-------------------------------------------------------------
    if [ $cycon -eq 1 -a $inest -eq 0 ];then
      if [ $hr3bkt -ne 0 -a $check -eq 0 ];then
        mk3p=3
        ppgm=make
      fi
    else
      hr3bkt=0
      if [ $rg = dgx ];then 
       if [ $check6 -eq 0 ];then
         mk3p=3;ppgm=make;hr3bkt=3   # create dgex 3 hr precip at 6 hr time intervals
       else
         if [ `expr $fhr - $fhrstr` -gt 3 ];then mk6p=6;ppgm=make;fi
       fi
      fi
    fi #cycon check
  fi  #fhr -ne 0

#------------------------------------------------------------------------------
# ON-CYCLE:  At 12-hr times:  Need 6 hour buckets as well
# Except for 6 hr times (18,30,42...) : Already have 6 hour buckets
# In addition, For 00/12 UTC valid times: Need to make 12 hour accumulations
#------------------------------------------------------------------------------
  case $fhr in 
    ${A6HR[0]}|${A6HR[1]}|${A6HR[2]}|${A6HR[3]}|${A6HR[4]}|${A6HR[5]}|${A6HR[6]}| \
    ${A6HR[7]}|${A6HR[8]} )
    if [ $cycon -eq 1 -a inest -eq 0 ];then
      mk6p=6
      ppgm=make
    else
#     off-cycles and  Nests have 3 hr buckets but need 6,12 hour precip
      mk6p=6
      mk12p=12
      ppgm=add
      if [ $rg = dgx ];then mk6p=0;fi   #dgex already has 6 hr precip in std wrfprs file
    fi;;
  esac 

#=====================================================================================
#    Link input model files where previous 3 or 6 hour bucket are found and run smartprecip
#=====================================================================================
  echo MKPCP Flags: MK3P $mk3p   MK6P $mk6p   MK12P $mk12p
  for MKPCP in $mk3p $mk6p $mk12p;do
    if [ $MKPCP -ne 0 ];then
      echo BEGIN Making $MKPCP hr PRECIP Buckets for $fhr Hour $ppgm freq $freq
      pfhr3=-99;pfhr4=-99
      case $MKPCP in
        $mk3p )
          FHRFRQ=$fhr3;freq=3
          pfhr1=$fhr;pfhr2=$fhr3;;

        $mk6p )
          FHRFRQ=$fhr6;freq=6
          pfhr1=$fhr;pfhr2=$fhr6    # fhr-fhr6
          if [ $ppgm = add ];then 
            FHRFRQ=$fhr3
            pfhr1=$fhr3;pfhr2=$fhr  # fhr3+fhr
          fi
          if [ $rg = dgx ];then
            ppgm=addsub
            pfhr1=$fhr;pfhr2=$fhr3;pfhr3=$fhr6         # fhr + (fhr3-fhr6)
            ln -sf "WRFPRS${fhr3}.tm00"     fort.17    # Will contain 6 hr precip
            ln -sf "WRFPRS${fhr3}i.tm00"    fort.18
          fi;;

        $mk12p )
          FHRFRQ=$fhr9;freq=12
          pfhr1=$fhr9;pfhr2=$fhr6;pfhr3=$fhr3;pfhr4=$fhr
          if [ $rg = dgx ];then 
#           DGEX 12 hr precip = Precip@fh6 + Precip@fhr   
            ppgm=add
            FHRFRQ=$fhr6
            pfhr1=$fhr6;pfhr2=$fhr;pfhr3=-99;pfhr4=-99
          fi;;  
      esac
      cp ${mdlin}${FHRFRQ}${text} WRFPRS${FHRFRQ}.tm00
      case $natgrd in bgrd3d) 
        ${utilexec}/wgrib -s WRFPRS${FHRFRQ}.tm00 |grep -f ${PARMdng}/${mdl}_smartinit.parmlist | \
        ${utilexec}/wgrib -i -grib -o temp WRFPRS${FHRFRQ}.tm00 > wgrib.out
        mv temp WRFPRS${FHRFRQ}.tm00;;
      esac
      $utilexec/grbindex WRFPRS${fhr}.tm00 WRFPRS${fhr}i.tm00
      $utilexec/grbindex WRFPRS${FHRFRQ}.tm00 WRFPRS${FHRFRQ}i.tm00

      export pgm=nam_smartprecip; . prep_step
      ln -sf "WRFPRS${FHRFRQ}.tm00"  fort.13  
      ln -sf "WRFPRS${FHRFRQ}i.tm00" fort.14
      ln -sf "WRFPRS${fhr}.tm00"     fort.15
      ln -sf "WRFPRS${fhr}i.tm00"    fort.16
      ln -sf "${freq}precip.${fhr}"  fort.50
      ln -sf "${freq}cprecip.${fhr}" fort.51
      ln -sf "${freq}snow.${fhr}"    fort.52

      if [ $MKPCP -eq $mk12p -a $pfhr4 -gt 0 ];then
        cp ${mdlin}${fhr3}${text} WRFPRS${fhr3}.tm00
        case $natgrd in bgrd3d) 
          ${utilexec}/wgrib -s WRFPRS${fhr3}.tm00 |grep -f ${PARMdng}/nam_smartinit.parmlist | \
          ${utilexec}/wgrib -i -grib -o temp WRFPRS${fhr3}.tm00 > wgrib.out
          mv temp WRFPRS${fhr3}.tm00;;
        esac
        $utilexec/grbindex WRFPRS${fhr3}.tm00 WRFPRS${fhr3}i.tm00

        cp ${mdlin}${fhr6}${text} WRFPRS${fhr6}.tm00
        case $natgrd in bgrd3d) 
          ${utilexec}/wgrib -s WRFPRS${fhr6}.tm00 |grep -f ${PARMdng}/nam_smartinit.parmlist | \
          ${utilexec}/wgrib -i -grib -o temp WRFPRS${fhr6}.tm00 > wgrib.out
          mv temp WRFPRS${fhr6}.tm00;;
        esac
        $utilexec/grbindex WRFPRS${fhr6}.tm00 WRFPRS${fhr6}i.tm00

        ln -sf "WRFPRS${fhr6}.tm00"      fort.15    
        ln -sf "WRFPRS${fhr6}i.tm00"     fort.16
        ln -sf "WRFPRS${fhr3}.tm00"      fort.17
        ln -sf "WRFPRS${fhr3}i.tm00"     fort.18
        ln -sf "WRFPRS${fhr}.tm00"       fort.19
        ln -sf "WRFPRS${fhr}i.tm00"      fort.20
      fi  # mk12p

#========================================================================
# nam_smartprecip : Create special Precip Bucket files for smartinit 
#========================================================================
      echo MAKE $freq HR PRECIP BUCKET FILE from fhrs $pfhr1 to $pfhr2 $pfhr3
      $EXECdng/nam_smartprecip <<EOF > ${ppgm}precip.out${fhr}
$pfhr1 $pfhr2 $pfhr3 $pfhr4 
EOF
      export err=$?;  err_chk

#     Interp precip to smartinit NDFD GRID
      cpgbgrd=$grid
      if [ $inest -gt 0 ];then cpgbgrd=$ogrd;fi
      if [ $RUNTYP = aknest3 ];then cpgbgrd=$grid;fi
      $utilexec/copygb -g "$cpgbgrd" -i3 -x ${freq}precip.${fhr} ${freq}precip
      $utilexec/grbindex ${freq}precip ${freq}precipi
      $utilexec/copygb -g "$cpgbgrd" -i3 -x ${freq}snow.${fhr} ${freq}snow
      $utilexec/grbindex ${freq}snow ${freq}snowi
    fi #MKPCP>0
  done #MKPCP loop

