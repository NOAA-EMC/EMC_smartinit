#!/bin/ksh 
#
# Author:        Geoff Manikin       Org: NP22         Date: 2007-08-06
#
# Script history log:
# 2007-08-06  Geoff Manikin
# 2012-07-24  Jeff McQueen  cleaned up redundant codes
#    Created precip threshold loop for creating sref prob precip files
# 2012-09-26  JTM : Modified for wcoss
#                   Combined on & off cycles options into one script 
# 2012-10-22  JTM : Combined addprecip and makeprecip codes
# 2012-10-26  JTM : Combined various nam region scripts
#                   smartinit: getgrib.f: fixed bug with reading sref prob file
# 2012-10-31  JTM : moved smartinit system to tide
# 2012-12-03  JTM : Unified for nam parent and nested region runs
# 2013-07-01  JTM : added extended conus (187) and guamnmmb, guamarw hrw options (199)
# 2013-07-03  JTM : Adding use of cfg file for grid settup
# 2013-08-27  JTM : Put in Vertical Structure
# 2013-11-20  JTM : Added option to downscale DGEX 3 hrly files beyond 84 hrs w/ 6 hr precip
# 2014-01-20  JTM : completed option to downscale hiresw guamnmmb, guamarw to 48 hours 
# 2016-08-24  Annette Gibbs : converted smartinit from GRIB1 to GRIB2; coincide with NAM upgrade
# 2025-07-18  Annette Gibbs : converted smartinit from NAM/NAMnest to RRFS; coincide with RRFS implementation
# 2025-10-14  Annette Gibbs : cleaned up scripts; removed DGEX/SREF/HIRESw/NAM
#======================================================================
#  Set Defaults fcst hours,cycle,model,region in smart_config_para called in parent job

# RUNTYP: OUTPUT REGION TO DOWNSCALE TO  (IN SMINIT.CTL FILE)
#=================================================================
# conusnest2p5 :  RRFS NA grid     NDFD-GRD=184/187/wexp
# priconest    :  RRFS NA grid     NDFD-GRD=195
# hawaiinest   :  RRFS NA grid     NDFD-GRD=196  
# aknest3      :  RRFS NA grid     NDFD-GRD=91


#======================================================================
# Check if this is a nest run

set -xa

# added inest=1, since all of RRFS smartinit is a nest
export inest=1

# Smartinit is grib2; remove all grib1
export grib=2

export rg=`echo $RUNTYP |cut -c1-2` 
echo IVADJ $IVADJ
export today=`$NDATE |cut -c 1-8`
#=====================================================================
# Set special filename extensions for mdl and output files
# mdl input file         : mdlgrd,natgrd
# eg: rrfs.t12z.natlev.3km.f001.na.grib2

# smartinit in/out file  : rg / outreg
# eg: MESOAK.NDFD
# eg: rrfs.t12z.smartconus2p501.tm00.grib2
#=====================================================================

# READ IN GRID INFO
linemax=`cat SMINIT.CTL |wc -l`
echo SMINIT  $linemax
let iline=0
while [ $iline -le $linemax ];do
  head -n $iline SMINIT.CTL >tempfile
  line=`tail -n1 tempfile`
  let k=1
  for word in $line; do
    case $k in
      1)  RCFG=$word;;
      2)  export rg=$word;;
      3)  export outreg=$word;;
      4)  export mdlgrd=$word;;
      5)  export sgrb=$word;;
      6)  export natgrd=$word;;
      7)  export ogrd=$word;;
      8)  gtyp=$word;;
      9)  gnx=$word;;
     10)  gny=$word;;
     11)  glat1=$word;;
     12)  glon1=$word;;
     13)  gdt=$word;;
     14)  tlon=$word;;
     15)  dx=$word;;
     16)  dy=$word;;
    esac
    let k=k+1
  done
  if [ $RUNTYP = "$RCFG" ];then
    echo GTYP $gtyp OGRD $ogrd
    if [ "$gtyp" = "$ogrd" ];then 
      export grid=$ogrd
    else
      export grid="$gtyp $gnx $gny $glat1 $glon1 $gdt $tlon $dx $dy"
    fi
    break
  else
    let iline=iline+1
    if [ $iline -gt $linemax ];then
      echo;echo  $RUNTYP not found in SMINIT.CTL file
      echo  EXITING SMARTINIT; exit
    fi
  fi
done

compress="c3 -set_bitmap 1"

case $RUNTYP in
    hawaiinest ) compress="jpeg -set_bitmap 1";;
    priconest ) compress="jpeg -set_bitmap 1";;  
esac


case $RUNTYP in
   conusnest2p5) natgrd=natlev; mdlgrd=conusnest; rg=con; outreg=conus2p5; wgrib2def="lambert:265:25:25 233.723448:2345:2539.703 19.228976:1597:2539.703";;
   hawaiinest) inest=1; rg=hi; natgrd=natlev; mdlgrd=hawaiinest; outreg=hi; wgrib2def="mercator:20 198.474999:321:2500:206.130999 18.072699:225:2500:23.087799";;
   priconest) inest=1; natgrd=natlev; rg=pr; mdlgrd=priconest; outreg=pr; wgrib2def="mercator:20.000000 291.804700:353:1250.000000:296.015500 16.828700:257:1250.000000:19.736200";;
   aknest3) natgrd=natlev; mdlgrd=alaskanest; rg=ak3; outreg=ak3; wgrib2def="nps:210:60 181.429:1649:2976.563 40.530101:1105:2976.563";;
esac

 
text=".tm00"

# For expanded conus nest 2.5 km
exptext=".grib2"
#case $RUNTYP in conusnest2p5) exptext="_grb188";; esac
case $RUNTYP in conusnest2p5) exptext="_wexp.grib2";; esac

# Define core 
core=$rg

prdgfl=meso${rg}.NDFD  # output prdgen grid name (eg: mesocon.NDFD,mesoak...)

cycon=0
case $cyc in 
   00|12 ) cycon=1;; 
esac

# Set forecast hours to compute 12 hour max/min Temps and 12 hr accum precip
# 12 hour max/mins must be computed at 00 and 12 UTC

# FOR NESTS,parent script, exnam, sets forecast range (60 or 54h)
case $cyc in
  00|12) set -A A6HR 12 24 36 48 60 72 84 96 108 120;;
  * )    set -A A6HR 18 30 42 54 66 78 90 102 114 126;;
esac
#======================================================================
#  Configure input met grib, land-sea mask and topo file names
#======================================================================

#  Set indices to determine input met file name 
#       eg: MDL.tCYCz.MDLGRD.NATGRD${FHR}.tm00
#       eg: nam.t12z.bgrd3d24.tm00
#       eg: nam.t12z.conusnest.bsmart24.tm00

#-------------------------------------------------------------------------
#   For all grids, set the following in NAM_SMINIT.CTL:
#   sgrb : Input SREF grid grib number (eg: 212, 216, 243); not used in RRFS smartinit
#   grid : output grid to copygb sref precip and nam precip buckets to 
#          one exception for non-nests where nam precip buckets are 
#          interpolated to smartinit output (ogrd)
#   ogrd : output grib number for prdgen and smartinit codes
#          Also used fo nest copygb interpolation
#          (eg: 197,196,195,198,184)
#  gtyp  : output grid w3fi63 grid type indicator (for copygb); not used in RRFS Smartinit
#       1: Mercator
#       3: Lambert Conformal
#       5: Polar Stereographic
#--------------------------------------------------------------------------

# Set NDFD output grid topo and land mask filenames
maskpre=${mdl}_smartmask${outreg}
topopre=${mdl}_smarttopo${outreg}
ext=grb2
maskfl=${maskpre}.${ext}
topofl=${topopre}.${ext}

echo
echo "============================================================================"
echo BEGIN SMARTINIT PROCESSING FOR FFHR $ffhr  CYCLE $cyc
echo RUNTYP:  $RUNTYP mdlgrd: $mdlgrd  rg: $rg
echo INPUT MDL DIR : $COMINrrfs
echo OUTPUT GRID: $ogrd $outreg
echo INTERP GRID for wgrib2 : $wgrib2def
echo OUTPUT GRID: $outreg
echo "============================================================"
echo 

let ffhr1=ffhr-1
let ffhr2=ffhr-2
hours="${ffhr}"
if [ $ffhr -ge 3 ];then hours="${ffhr2} ${ffhr1} ${ffhr}";fi

#===========================================================
#  CREATE Accum precip buckets if necessary 
#===========================================================
for fhr in $hours; do
  rm -f *out${fhr}
  mk3p=0;mk6p=0;mk12p=0
  let check=fhr%3
  let check6=fhr%6
  if [ $cycon -eq 1 ];then
    let check12=fhr%12
  else
    let check12=$((fhr-18))%12
  fi
  echo check check6 check12 $check $check6 $check12
  let fhr1=fhr-1
  let fhr2=fhr-2
  let fhr3=fhr-3
  let fhr6=fhr-6
  let fhr9=fhr-9
  let fhr12=fhr-12
  if [ $fhr -gt 00 ];then 
   if [ $fhr -lt 10 -a $check -ne 0 ];then fhr="0"${fhr};fi
   if [ $fhr1 -lt 10 ];then fhr1="0"${fhr1};fi
   if [ $fhr2 -lt 10 ];then fhr2="0"${fhr2};fi
   if [ $fhr3 -lt 10 ];then fhr3="0"${fhr3};fi
   if [ $fhr6 -lt 10 ];then fhr6="0"${fhr6};fi
   if [ $fhr9 -lt 10 ];then fhr9="0"${fhr9};fi
   if [ $fhr12 -lt 10 ];then fhr12="0"${fhr12};fi
  fi
  echo FHR FHR1 FHR2 FHR3 FHR6 FHR9 FHR12  $fhr $fhr1 $fhr2 $fhr3 $fhr6 $fhr9 $fhr12


# Get cloud ceiling height from 2dfld file; it is no longer in the prslev file
# Get TMP at 950,850,700,500 mb and RH at 850,700 mb from prslev file; it is no longer in natlev file
  mdlin=$COMINrrfs/${cyc}/${mdl}.t${cyc}z.${natgrd}
  wgrib2 $COMINrrfs/${cyc}/${mdl}.t${cyc}z.2dfld.3km.f0${fhr}.na.grib2 -match "HGT:cloud ceiling" -grib 2dfld_fields${fhr}.grib2
  wgrib2 $COMINrrfs/${cyc}/${mdl}.t${cyc}z.prslev.3km.f0${fhr}.na.grib2 -match ":(TMP:(950|850|700|500) mb|RH:(850|700) mb):" -grib prslev_fields${fhr}.grib2
  cp ${mdlin}.3km.f0${fhr}.na.grib2 WRFPRS${fhr}.tm00
  cat prslev_fields${fhr}.grib2 2dfld_fields${fhr}.grib2 >> WRFPRS${fhr}.tm00

  inhrfrq=1

  if [ $fhr -gt ${fhrstr} ];then

# sminit_mkprcp.sh ######################################
#-------------------------------------------------------------
#   NAM - OFF-CYC & Nests: Create 6/12 hour buckets, 3 hr buckets available
#   RRFS - OFF-CYC : Create all buckets (3/6/12 hour), only model forecast totals available
#   ON-CYC :
#     NAM 3hr precip available at only 3,15, 27,39... forcast fhours
#     other hours, create 3 hr precip
#     6hr precip: Create only at 00/12 UTC valid times 
#           eg: fhr=12,24,36
#     Create 12 hour precip at 00/12 UTC valid times
#-------------------------------------------------------------
    mk3p=0
    if [ $check6 -eq 0 -a $fhr -gt 6 ];then 
      mk6p=6
      ppgm=make #RRFS
    fi

#-------------------------------------------------------------
#   ON-CYC: The 3-hr fhrs (3,15,27,39....) --> Already have 3-hr buckets
#   For in-between fhrs (22,23,25) --> Create 3-hr buckets
#   since we only gather max/min data at those hours to compute 12 hr max/mins
#-------------------------------------------------------------
#-------------------------------------------------------------
    if [ $check -eq 0 -a $fhr -gt 3 ];then
      mk3p=3
      ppgm=make
    fi
    if [ $check12 -eq 0 -a $fhr -gt 12 ];then
      mk12p=12
      ppgm=make
    fi
  fi  #fhr -ne 0

  echo MKPCP Flags: MK3P $mk3p   MK6P $mk6p   MK12P $mk12p
  for MKPCP in $mk3p $mk6p $mk12p;do
    if [ $MKPCP -ne 0 ];then
      echo ====================================================================
      echo BEGIN Making $MKPCP hr PRECIP Buckets for $fhr Hour $ppgm freq $freq
      echo ====================================================================
      pfhr3=-99;pfhr4=-99
      case $MKPCP in
        $mk3p )
          FHRFRQ=$fhr3;freq=3
          pfhr1=$fhr;pfhr2=$fhr3;;

        $mk6p )
          FHRFRQ=$fhr6;freq=6
          pfhr1=$fhr;pfhr2=$fhr6;;

        $mk12p )
          FHRFRQ=$fhr12;freq=12
          pfhr1=$fhr;pfhr2=$fhr12;;
      esac
      cp ${mdlin}.3km.f0${FHRFRQ}.na.grib2 WRFPRS${FHRFRQ}.tm00

      if [ $fhr -eq 24 -o $fhr -eq 48 -o $fhr -eq 72 ]; then
        fday=$((fhr / 24))
        $WGRIB2 WRFPRS${fhr}.tm00 -match "(APCP|TSNOWP):surface:0-${fday} day acc fcst:" -grib WRFPRS${fhr}.tm00.precip_only
      else
        $WGRIB2 WRFPRS${fhr}.tm00 -match "(APCP|TSNOWP):surface:0-${fhr#0} hour acc fcst:" -grib WRFPRS${fhr}.tm00.precip_only
      fi
      $GRB2INDEX WRFPRS${fhr}.tm00.precip_only WRFPRS${fhr}i.tm00.precip_only
      if [ $FHRFRQ -eq 24 -o $FHRFRQ -eq 48 -o $FHRFRQ -eq 72 ]; then
        fday=$((FHRFRQ / 24))
        $WGRIB2 WRFPRS${FHRFRQ}.tm00 -match "(APCP|TSNOWP):surface:0-${fday} day acc fcst:" -grib WRFPRS${FHRFRQ}.tm00.precip_only
      else
        $WGRIB2 WRFPRS${FHRFRQ}.tm00 -match "(APCP|TSNOWP):surface:0-${FHRFRQ#0} hour acc fcst:" -grib WRFPRS${FHRFRQ}.tm00.precip_only
      fi
      $GRB2INDEX WRFPRS${FHRFRQ}.tm00.precip_only WRFPRS${FHRFRQ}i.tm00.precip_only

      export pgm=smartprecip_g2; . prep_step
      ln -sf "WRFPRS${FHRFRQ}.tm00.precip_only"  fort.13  
      ln -sf "WRFPRS${FHRFRQ}i.tm00.precip_only" fort.14
      ln -sf "WRFPRS${fhr}.tm00.precip_only"     fort.15
      ln -sf "WRFPRS${fhr}i.tm00.precip_only"    fort.16
      ln -sf "${freq}precip.${fhr}"  fort.50
#     ln -sf "${freq}cprecip.${fhr}" fort.51
      ln -sf "${freq}snow.${fhr}"    fort.52

#===============================================================
# smartprecip : Create Precip Buckets for smartinit 
#===============================================================
      echo MAKE $freq HR PRECIP BUCKET FILE from fhrs $pfhr1 to $pfhr2 $pfhr3
      $EXECdng/smartprecip_g2 <<EOF > ${ppgm}precip${freq}.out
$pfhr1 $pfhr2 $pfhr3 $pfhr4 
EOF
      export err=$?;  err_chk

# Matt uses neighbor, but original code uses budget
      $WGRIB2 ${freq}precip.${fhr} -set_grib_type ${compress} -new_grid_winds grid -new_grid_interpolation budget -new_grid ${wgrib2def}  ${freq}precip
      $GRB2INDEX ${freq}precip ${freq}precipi
      $WGRIB2 ${freq}snow.${fhr} -set_grib_type ${compress} -new_grid_winds grid -new_grid_interpolation budget -new_grid ${wgrib2def}  ${freq}snow
      $GRB2INDEX ${freq}snow ${freq}snowi

    fi #MKPCP>0
  done #MKPCP loop

# Begin bi-linear interpolation to NDFD grid

 interp="-new_grid_interpolation bilinear"

# Begin parallel wgrib2

  if [ -e inputs.grb2_1 ]
  then
    rm inputs.grb2*
  fi

  if [ -e model.ndfd_1 ]
  then
    rm model.ndfd*
  fi

  if [ -e wgrib2.poe ];
  then
    rm wgrib2.poe
  fi

  ngrd=$natgrd

  if [ $CFP = 'YES' -a $NTASKS -eq 24 ]; then
    cp -p $PARMdng/smartinit_${ngrd}_grb2.parmlist natlev.txt
    sed -n -e '1,12p' natlev.txt > inventory.txt1
    sed -n -e '13,22p' natlev.txt > inventory.txt2
    sed -n -e '23,33p' natlev.txt > inventory.txt3
    sed -n -e '34,44p' natlev.txt > inventory.txt4
    sed -n -e '45,55p' natlev.txt > inventory.txt5
    sed -n -e '56,66p' natlev.txt > inventory.txt6
    sed -n -e '67,78p' natlev.txt > inventory.txt7
    sed -n -e '79,88p' natlev.txt > inventory.txt8
    sed -n -e '89,99p' natlev.txt > inventory.txt9 
    sed -n -e '100,110p' natlev.txt > inventory.txt10
    sed -n -e '111,121p' natlev.txt > inventory.txt11
    sed -n -e '122,132p' natlev.txt > inventory.txt12
    sed -n -e '133,144p' natlev.txt > inventory.txt13
    sed -n -e '145,154p' natlev.txt > inventory.txt14
    sed -n -e '155,165p' natlev.txt > inventory.txt15
    sed -n -e '166,176p' natlev.txt > inventory.txt16
    sed -n -e '177,188p' natlev.txt > inventory.txt17
    sed -n -e '189,200p' natlev.txt > inventory.txt18
    sed -n -e '201,212p' natlev.txt > inventory.txt19
    sed -n -e '213,224p' natlev.txt > inventory.txt20
    sed -n -e '225,236p' natlev.txt > inventory.txt21
    sed -n -e '237,248p' natlev.txt > inventory.txt22
    sed -n -e '249,260p' natlev.txt > inventory.txt23
    sed -n -e '261,$p' natlev.txt > inventory.txt24

    tasks=(24)
    count=0
    for task in $(seq ${tasks[count]})
    do
      echo "${USHdng}/smartinit_subpiece.sh ${task} ${DATA} WRFPRS${fhr}.tm00 \"${wgrib2def}\" \"${compress}\" \"${interp}\" " >> wgrib2.poe
    done
    count=$count+1

    chmod 775 wgrib2.poe
    export MP_CMDFILE=wgrib2.poe

# parallel execution - only tested 24 tasks
    time mpiexec -np $NTASKS --cpu-bind core cfp $MP_CMDFILE
    export err=$?;  err_chk

# reassemble the grid
    tasks=(24)
    count=0
    for task in $(seq ${tasks[count]})
    do
      cat $DATA/model.ndfd_${task} >> ${prdgfl}
    done
    count=$count+1

  else
    # serial processing

    cp -p $PARMdng/smartinit_${ngrd}_grb2.parmlist natlev.txt
    cp -p $PARMdng/smartinit_${ngrd}_grb2.parmlist_1 inventory.txt1
    cp -p $PARMdng/smartinit_${ngrd}_grb2.parmlist_2 inventory.txt2
    cp -p $PARMdng/smartinit_${ngrd}_grb2.parmlist_3 inventory.txt3
    cp -p $PARMdng/smartinit_${ngrd}_grb2.parmlist_4 inventory.txt4
    cp -p $PARMdng/smartinit_${ngrd}_grb2.parmlist_5 inventory.txt5
    cp -p $PARMdng/smartinit_${ngrd}_grb2.parmlist_6 inventory.txt6
    cp -p $PARMdng/smartinit_${ngrd}_grb2.parmlist_7 inventory.txt7
    cp -p $PARMdng/smartinit_${ngrd}_grb2.parmlist_8 inventory.txt8
    cp -p $PARMdng/smartinit_${ngrd}_grb2.parmlist_9 inventory.txt9
    cp -p $PARMdng/smartinit_${ngrd}_grb2.parmlist_10 inventory.txt10
    cp -p $PARMdng/smartinit_grb2_budget.parmlist_1 inventoryb.txt1
    cp -p $PARMdng/smartinit_grb2_budget.parmlist_2 inventoryb.txt2
    cp -p $PARMdng/smartinit_grb2_nn.parmlist inventoryn.txt

    $WGRIB2 WRFPRS${fhr}.tm00 | grep -F -f inventory.txt1 | $WGRIB2 -i -grib inputs.grb2_1 WRFPRS${fhr}.tm00
    $WGRIB2 WRFPRS${fhr}.tm00 | grep -F -f inventory.txt2 | $WGRIB2 -i -grib inputs.grb2_2 WRFPRS${fhr}.tm00
    $WGRIB2 WRFPRS${fhr}.tm00 | grep -F -f inventory.txt3 | $WGRIB2 -i -grib inputs.grb2_3 WRFPRS${fhr}.tm00
    $WGRIB2 WRFPRS${fhr}.tm00 | grep -F -f inventory.txt4 | $WGRIB2 -i -grib inputs.grb2_4 WRFPRS${fhr}.tm00
    $WGRIB2 WRFPRS${fhr}.tm00 | grep -F -f inventory.txt5 | $WGRIB2 -i -grib inputs.grb2_5 WRFPRS${fhr}.tm00
    $WGRIB2 WRFPRS${fhr}.tm00 | grep -F -f inventory.txt6 | $WGRIB2 -i -grib inputs.grb2_6 WRFPRS${fhr}.tm00
    $WGRIB2 WRFPRS${fhr}.tm00 | grep -F -f inventory.txt7 | $WGRIB2 -i -grib inputs.grb2_7 WRFPRS${fhr}.tm00
    $WGRIB2 WRFPRS${fhr}.tm00 | grep -F -f inventory.txt8 | $WGRIB2 -i -grib inputs.grb2_8 WRFPRS${fhr}.tm00
    $WGRIB2 WRFPRS${fhr}.tm00 | grep -F -f inventory.txt9 | $WGRIB2 -i -grib inputs.grb2_9 WRFPRS${fhr}.tm00
    $WGRIB2 WRFPRS${fhr}.tm00 | grep -F -f inventory.txt10 | $WGRIB2 -i -grib inputs.grb2_10 WRFPRS${fhr}.tm00
    $WGRIB2 WRFPRS${fhr}.tm00 | grep -F -f inventoryb.txt1 | $WGRIB2 -i -grib inputsb.grb2_1 WRFPRS${fhr}.tm00
    $WGRIB2 WRFPRS${fhr}.tm00 | grep -F -f inventoryb.txt2 | $WGRIB2 -i -grib inputsb.grb2_2 WRFPRS${fhr}.tm00
    $WGRIB2 WRFPRS${fhr}.tm00 | grep -F -f inventoryn.txt | $WGRIB2 -i -grib inputsn.grb2 WRFPRS${fhr}.tm00

    echo "#! /bin/ksh" > a.poe
    echo "$WGRIB2 inputs.grb2_1 -set_grib_type ${compress} -new_grid_winds grid ${interp} -new_grid ${wgrib2def} model.ndfd_1" >> a.poe
    echo "#! /bin/ksh" > b.poe
    echo "$WGRIB2 inputs.grb2_2 -set_grib_type ${compress} -new_grid_winds grid ${interp} -new_grid ${wgrib2def} model.ndfd_2" >> b.poe
    echo "#! /bin/ksh" > c.poe
    echo "$WGRIB2 inputs.grb2_3 -set_grib_type ${compress} -new_grid_winds grid ${interp} -new_grid ${wgrib2def} model.ndfd_3" >> c.poe
    echo "#! /bin/ksh" > d.poe
    echo "$WGRIB2 inputs.grb2_4 -set_grib_type ${compress} -new_grid_winds grid ${interp} -new_grid ${wgrib2def} model.ndfd_4" >> d.poe
    echo "#! /bin/ksh" > e.poe
    echo "$WGRIB2 inputs.grb2_5 -set_grib_type ${compress} -new_grid_winds grid ${interp} -new_grid ${wgrib2def} model.ndfd_5" >> e.poe
    echo "#! /bin/ksh" > f.poe
    echo "$WGRIB2 inputs.grb2_6 -set_grib_type ${compress} -new_grid_winds grid ${interp} -new_grid ${wgrib2def} model.ndfd_6" >> f.poe
    echo "#! /bin/ksh" > g.poe
    echo "$WGRIB2 inputs.grb2_7 -set_grib_type ${compress} -new_grid_winds grid ${interp} -new_grid ${wgrib2def} model.ndfd_7" >> g.poe
    echo "#! /bin/ksh" > h.poe
    echo "$WGRIB2 inputs.grb2_8 -set_grib_type ${compress} -new_grid_winds grid ${interp} -new_grid ${wgrib2def} model.ndfd_8" >> h.poe
    echo "#! /bin/ksh" > i.poe
    echo "$WGRIB2 inputs.grb2_9 -set_grib_type ${compress} -new_grid_winds grid ${interp} -new_grid ${wgrib2def} model.ndfd_9" >> i.poe
    echo "#! /bin/ksh" > j.poe
    echo "$WGRIB2 inputs.grb2_10 -set_grib_type ${compress} -new_grid_winds grid ${interp} -new_grid ${wgrib2def} model.ndfd_10" >> j.poe

# always use budget interpolation for precip and snow
# but GRIB1 smartinit uses bilinear interpolation

    interp="-new_grid_interpolation bilinear"
    echo "#! /bin/ksh" > k.poe
    echo "$WGRIB2 inputsb.grb2_1 -set_grib_type ${compress} -new_grid_winds grid ${interp} -new_grid ${wgrib2def} model.ndfd_b1" >> k.poe
    echo "#! /bin/ksh" > l.poe
    echo "$WGRIB2 inputsb.grb2_2 -set_grib_type ${compress} -new_grid_winds grid ${interp} -new_grid ${wgrib2def} model.ndfd_b2" >> l.poe

# always use nearest neighbor interpolation for these fields

    interp="-new_grid_interpolation neighbor"
    $WGRIB2 inputsn.grb2 -set_grib_type ${compress} -new_grid_winds grid ${interp} -new_grid ${wgrib2def} model.ndfd_n

    chmod 775 ?.poe

# cfp/serial syntax
    echo "./a.poe" > wgrib2.poe
    echo "./b.poe" >> wgrib2.poe
    echo "./c.poe" >> wgrib2.poe
    echo "./d.poe" >> wgrib2.poe
    echo "./e.poe" >> wgrib2.poe
    echo "./f.poe" >> wgrib2.poe
    echo "./g.poe" >> wgrib2.poe
    echo "./h.poe" >> wgrib2.poe
    echo "./i.poe" >> wgrib2.poe
    echo "./j.poe" >> wgrib2.poe
    echo "./k.poe" >> wgrib2.poe
    echo "./l.poe" >> wgrib2.poe

    chmod 775 wgrib2.poe
    export MP_CMDFILE=wgrib2.poe

  # serial execution
    time ./$MP_CMDFILE
    export err=$?;  err_chk

    cat model.ndfd_1 model.ndfd_2 model.ndfd_3 model.ndfd_4 model.ndfd_5 model.ndfd_6 \
      model.ndfd_7 model.ndfd_8 model.ndfd_9 model.ndfd_10 model.ndfd_b1 model.ndfd_b2 model.ndfd_n > ${prdgfl}
  fi

  if [ $PDY = $today ];then
    cp ${COMROOT}/date/t${cyc}z DATE
  else
    echo "DATE  "${PDY}${cyc}"00WASHINGTON" >DATE
  fi
  if [ -s $prdgfl ];then  
    echo $prdgfl FOUND FOR FORECAST HOUR ${fhr}
    $WGRIB2 ${prdgfl} -not_if "(APCP|TSNOWP):surface:${fhr1#0}-${fhr#0} hour acc fcst:" -grib meso${rg}.NDFDf${fhr}
    rm ${prdgfl}
  elif [ -s ${prdgfl}${fhr} ];then    # check for hawaii ???
    echo $prdgfl${fhr} FOUND FOR FORECAST HOUR ${fhr}
    mv ${prdgfl}${fhr} meso${rg}.NDFDf${fhr}  
  else
    echo $prdgfl NOT FOUND FOR FORECAST HOUR ${fhr}
    export err=2
    err_chk
  fi
  $GRB2INDEX meso${rg}.NDFDf${fhr} meso${rg}.NDFDif${fhr}
#=================================================================
#   DECLARE INPUTS and RUN SMARTINIT 
#=================================================================

  cp $FIXdng/topo/${topofl} TOPONDFD
  cp $FIXdng/mask/${maskfl} LANDNDFD
  ln -sf TOPONDFD     fort.46
  ln -sf LANDNDFD     fort.48
  $GRB2INDEX TOPONDFD TOPONDFDi
  $GRB2INDEX LANDNDFD LANDNDFDi
  ln -sf TOPONDFDi  fort.47
  ln -sf LANDNDFDi  fort.49

  mksmart=1
  if [ $check -eq 0 -a $fhr -ne $fhrstr ];then 
    if [ -s MAXMIN${fhr1}.tm00 ];then
      echo MAXMIN${fhr1}.tm00 FOUND
      cp MAXMIN${fhr2}.tm00 MAXMIN2
      cp MAXMIN${fhr1}.tm00 MAXMIN1
    fi
    $GRB2INDEX MAXMIN1 MAXMIN1i
    $GRB2INDEX MAXMIN2 MAXMIN2i
  fi

  ln -sf "meso${rg}.NDFDf${fhr}"    fort.11
  ln -sf "meso${rg}.NDFDif${fhr}"   fort.12
  if [ mk3p -ne 0 ]; then
    ln -sf "3precip"            fort.15
    ln -sf "3precipi"           fort.16
    ln -sf "3snow"      fort.17
    ln -sf "3snowi"     fort.18
  fi
  if [ mk6p -ne 0 ]; then
    ln -sf "6precip"            fort.19
    ln -sf "6precipi"           fort.20
    ln -sf "6snow"      fort.21
    ln -sf "6snowi"     fort.22
  fi

# At 12-hr times, input 12-hr max/min temps and 3 and 6-hr buckets
  case $fhr in 
    ${A6HR[0]}|${A6HR[1]}|${A6HR[2]}|${A6HR[3]}|${A6HR[4]}|${A6HR[5]}|${A6HR[6]}| \
    ${A6HR[7]}|${A6HR[8]})
    echo "********************************************************"
    echo RUN SMARTINIT for 12h valid 00 or 12Z fcst hours: $fhr

    cp $COMOUT/${mdl}.t${cyc}z.smart${outreg}${fhr3}.tm00${exptext} MAXMIN3
    cp $COMOUT/${mdl}.t${cyc}z.smart${outreg}${fhr6}.tm00${exptext} MAXMIN4
    cp $COMOUT/${mdl}.t${cyc}z.smart${outreg}${fhr9}.tm00${exptext} MAXMIN5
    $GRB2INDEX MAXMIN3 MAXMIN3i
    $GRB2INDEX MAXMIN4 MAXMIN4i
    $GRB2INDEX MAXMIN5 MAXMIN5i

    if [ $mk3p -ne 0 -a $mk6p -ne 0 -a $mk12p -ne 0 ]; then
      ln -sf "12precip"   fort.23
      ln -sf "12precipi"  fort.24
      fmx=25
    else
      fmx=23
    fi

    ln -sf "MAXMIN1"   fort.$fmx
    ln -sf "MAXMIN2"   fort.$((fmx+1))
    ln -sf "MAXMIN3"   fort.$((fmx+2))
    ln -sf "MAXMIN4"   fort.$((fmx+3))
    ln -sf "MAXMIN5"   fort.$((fmx+4))
    ln -sf "MAXMIN1i"  fort.$((fmx+5))
    ln -sf "MAXMIN2i"  fort.$((fmx+6))
    ln -sf "MAXMIN3i"  fort.$((fmx+7))
    ln -sf "MAXMIN4i"  fort.$((fmx+8))
    ln -sf "MAXMIN5i"  fort.$((fmx+9));;

    *)   # Not 00/12 UTC valid times
     if [ $check -eq 0 -a $fhr -ne $fhrstr ];then
       if [ $mk3p -ne 0 -a $fhr -ne $fhrstr -a $mk6p -ne 0 ];then
         echo "****************************************************************"
         case $cycon in
          1) echo RUN SMARTINIT for ON-CYC  hrs without 3 hr buckets : $fhr;;
          *) echo RUN SMARTINIT for OFF-CYC hrs without 6 hr buckets : $fhr;;
         esac
         ln -sf "MAXMIN2"   fort.23
         ln -sf "MAXMIN1"   fort.24
         ln -sf "MAXMIN2i"  fort.25
         ln -sf "MAXMIN1i"  fort.26

       else           
#        READ PRECIP FROM INPUT MDL GRIB FILE 
#        ON-CYC:  Forecast hours 3,15,27,39....already  have 3-hr buckets,
#        OFF-CYC: 3 hour buckets available for all 3 hour forecast times
#        ALL-CYC: Input only  max/min temp data for the previous 2 hours

         echo "****************************************************"
         echo RUN SMARTINIT for hours with 3 hr buckets: $fhr
         if [ $fhr -eq 3 ]; then
           ln -sf "MAXMIN2"   fort.15
           ln -sf "MAXMIN1"   fort.16
           ln -sf "MAXMIN2i"  fort.17
           ln -sf "MAXMIN1i"  fort.18
         else
           ln -sf "MAXMIN2"   fort.19
           ln -sf "MAXMIN1"   fort.20
           ln -sf "MAXMIN2i"  fort.21
           ln -sf "MAXMIN1i"  fort.22
         fi
       fi  

     else   # fhr%3 -ne 0
#      For all "in-between" forecast hours (13,14,16....)
#      No special data needed
       echo "*****************************************************"
       echo RUN SMARTINIT for in-between hour: $fhr
       ln -fs " " fort.13
       ln -fs " " fort.14
       ln -fs " " fort.15
       ln -fs " " fort.16
       mksmart=0
     fi;;
  esac

#========================================================
# Run SMARTINIT
#========================================================
  hrlyfhr=12  # forecast hour to output hourly files to
  case $RUNTYP in
      conusnest2p5) RGIN=CS2P;hrlyfhr=36;;
                 *) RGIN=`echo $rg |tr '[a-z]'  '[A-Z]' `;;
  esac

  export pgm=smartinit_g2; . prep_step
  ${EXECdng}/smartinit_g2 $cyc $fhr $ogrd $RGIN $inest $inhrfrq $fhrstr $core >smartinit.out${fhr}
  export err=$?; err_chk

  if [ -e smartinit.out${fhr} ]; then
    cat smartinit.out$fhr
  fi

# Save hourly ak,hi,pr,conus2p5 nests for RTMA 1st guess fields
  if [ $fhr -le $hrlyfhr ];then
    case $RUNTYP in
     hawaiinest|priconest|conusnest2p5|aknest3)
       mksmart=1
    esac
  fi

  if [ $mksmart -eq 1 ];then

#   Only create awips files every 3 hours [AMG]; if check = 0, its a 3-hourly time
    if [ $check -eq 0 ]; then
      let awpchk=0 # 3-hrly
    else
      let awpchk=1 # in-between hours
    fi
#   Do not create any awips files since public dissemination will be removed
    let awpchk=1
    echo $awpchk

#   Run NCO processing to convert output to grib2 and awips
    export RUNTYP
    export RGIN=$RGIN  # Region id (eg: CS, HI, PR,AK..)
    export outreg
    export cyc  
    export fhr=$fhr
    export ogrd 
    export mdl
    ${USHdng}/dng_awp_g2.sh $outreg $awpchk
  fi
done  #fhr loop

exit
