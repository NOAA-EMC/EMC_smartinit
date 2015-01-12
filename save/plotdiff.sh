#!/bin/ksh
echo "======================================================="

###set -x
#####################################################################
#  Script to produce NAWIPS gridded difference plots
#  Input files:
#     MDL GEMPAK pressure grib file: nam.gem
#    NOTE set nam file directory in var rootd below
#    NOTE set nam file name format in var mdlfile below
# 10/13: Allowed differencing of Varbs at diff glevels and coords
#        Check if first mdlin = nam, if so, ensure correct level/coord
#####################################################################
dev=GIF
# if [ -z "$plotarea" ];then plotarea=dset;fi

# Set ovly=r to plot topo on top of difference fields
ovly=
ovly3=

# rootd defined in parent job
rootd=/stmp/$USER

# export From parent job: 
# cycle: run cycle eg: 00, 06, 12, 18...
# fhr: forecast hour eg: from 0 to 84...
# VARB: GEMPAK VARIABLE NAME: eg: tmpc, dwpc, p06i...
# mdlin: mdls to plot diff: eg: nam, namy, aqm,obs
# reg:  mdltypes to plot diff: eg: conus,conusnest...
# glevel: variable vertical level

set -A mdlin ${mdlin[*]}
set -A reg ${reg[*]}

# Assume control directory name prefix is gem${can}
# 11-18-2013 if [ $plotarea = dset ];then
  set -A drt ${gpoint} ${gpoint}
#else
#  set -A drt ${plotarea} ${plotarea}
#fi
for plt in 0 1;do
  drt[plt]=`echo ${drt[plt]} |cut -d"*" -f1`
done

echo plotarea ${plotarea[*]}  gpoint $gpoint
echo MDLD $mdld DRT ${drt[*]}  cyc $cyc
#             CONTROL(d2)  vs EXP
set -A dirin ${rootd}/smartplt/d2${mdld}${drt[0]}${cyc}  ${rootd}/smartplt/${mdld}${drt[0]}${cyc}
echo DIRIN 1 ${dirin[0]} 
echo DIRIN 2 ${dirin[1]} 

#===========================================================
#=SET GRAPHICS DEFAULTS based on area: line, text, mapfile...
#===========================================================

# SET label for plot file name (gpoint)
#TEST gpoint=`echo ${reg[0]} |cut -c 1-3 `
# gpoint=${reg}

#TEXT size/font/width/border/rotation/justification/HW or SW
TTEXT="0.9/22/0.9/111///SW"

#LINE color/type/width/label frequency/smoothing/filter/suppress small contours 
line="1/1/3/0/2/2/T"

# COLOR BAR: color/V/Location/Starting x;y/box length;width/label location
clrbar="1/V/LL/.001;.3/.5;.02/-1"

mapf="HISTUS.NWS + HIPOWO.CIA "
filter=YES

skipv="/2;2"
izoom1=`echo $plotarea|awk '{ print( index($0,"*") )}'`
izoom2=`echo $plotarea|awk '{ print( index($0,"**") )}'`
izoom3=`echo $plotarea|awk '{ print( index($0,"***") )}'`
izoom4=`echo $plotarea|awk '{ print( index($0,"****") )}'`
izoom=1
if [ $izoom2 -gt 0 ];then latlon="4/8/3/1/1;1" ;fi
if [ $izoom3 -gt 0 ];then latlon="4/8/3/0.5/0.5;0.5" ;skipv="/1;1";izoom=2;fi
if [ $izoom4 -gt 0 ];then skipv=0;izoom=2;fi
case $gpoint in hi|pr|pu) izoom=1.5;;esac
stsz=$(echo "0.6*$izoom" |bc )
stwd=$(echo "0.8*$izoom" |bc )
echo IZOOM $izoom $izoom2 $izoom3 $stsz $stwd

# txt: clr/siz/font/wid/border | mrk clr/type/siz/wid/hw flag
stnplt="32/${stsz}//${stwd}/0|3|sfstns.tbl"

case ${gpoint} in
  dset|conus|conusnest|ak|ak3 ) skipv="/25;25";stnplt=;;
                 hi|phnl|pr|pu) skipv="/1;1";mapf=;;
                            * )
    mapf="LORVWO.CIA + HISTUS.NWS "
    filter=NO
    TTEXT="0.9/11/1.1/SW"
    latlon="4/8/3/1/2;2";;
esac

#=====END USER SETS============================================
typeset -Z2 cyc fhr avghr 

#  avghr, yyyymmdd defined in parent job
yymmdd=`echo $yyyymmdd | cut -c 3-8`
echo DATE to PLOT: $yyyymmdd
obsfile=${yyyymmdd}.sfc
vdate="${yyyymmdd}${cyc}"
export obsdate=`/nwprod/util/exec/ndate $fhr $vdate |cut -c 3-8 `
export obshr=`/nwprod/util/exec/ndate $fhr $vdate |cut -c 9-10`
export obshr="${obshr}00"

vhdrm=`echo $VARB |cut -c1-2`
export vhdr=`echo $VARB |cut -c1-4` 
if [ "$vhdrm" = "P0" ];then export vhdr=$VARB;fi

type=F
dir=`pwd |cut -d/ -f1`
#title="$dir _ valid  ~ "


#Set vertical coordinate for gemfile2  (smart)
oglvl=$glevel
HYBL=`echo $oglvl |cut -c 1-1`
echo HYBL $oglvl   varb=$VARB
ovarb=$VARB
oglvl=$glevel
ocord=$coord
case $oglvl in
             0 ) ocord=none;;
          2|10 ) ocord=HGHT;;
     [11-1099] ) if [ $HYBL != H ];then ocord=PRES;fi;;
  [1100-99999] ) ocord=SGMA;;
     PBLR|LLTW ) ocord=$oglvl;oglvl=0;;
            *  ) echo "HYBRID LEVEL ASSUMED" $glevel;;
esac
if [ $HYBL = H ];then 
  ocord=HYBL
  glevel=`echo $oglvl|cut -c 2-3` 
fi
slvl=`echo $oglvl |grep :`
if [ -n "$slvl" ];then ocord=DPTH;fi


oclear=n
FLINE="28-23;31;20-12"

case $vhdr in 
 TMPF|DWPF|TMPC|TMXK|TMNK|VSBY )
   ovly=r
   if [ $mdlin = nam ];then glevel=2;fi  # if 1st model is not smart, then change glevel from 0 to 2
   fint="-10;-8;-6;-4;-2;-1;1;2;4;6;8;10"
   FLINE="28-23;31;20-12"
   if [ $vhdr = DWPF ];then FLINE="14-19;31;20;5;3;21-25";fi;;
 SPFH )
   if [ $mdlin = nam ];then glevel=2;fi
   fint="-.025;-.020;-.015;-.010;-.005;-.002;.002;.005;.010;.015;.020;.025"
   FLINE="14-19;31;20;5;3;21-25";;
 RELH|MINR|MAXR )
   if [ ${mdlin} = nam ];then glevel=2;fi
   fint="-30;-25;-20;-15;-10;-5;5;10;15;20;25;30"
   FLINE="14-19;31;20;5;3;21-25";;
 SPED|GUST|SKNT )
   ovly=r;ovly3=r
   if [ $vhdr = SKNT ];then VARB="MUL(SPED,1.944)";fi
   if [ ${mdlin} = nam -a $vhdr = SPED ];then glevel=10;fi
   fint="-10;-8;-6;-4;-2;-1;1;2;4;6;8;10"
   FLINE="28-23;31;20-12";;
 DRCT )
   ovly=r;ovly3=r
   gdpfun=" dsub(WND @${oglvl} %${ocord},WND+2 @${oglvl} %${ocord}) "
   gdpfun2="kntv(wnd)"
   if [ ${mdlin} = nam -a $vhdr = SPED ];then glevel=10;fi
   fint="-135;-112.5;-90;-67.5;-45;-22.5;22.5;45;67.5;90;112.5;135"
   FLINE="28-23;31;20-12";;
 ZPBL |DIST )
   if [ $mdlin = nam ];then 
#    IF 1st model is not smart dng, then force varb1=ZPBL
     VARB=ZPBL    #NAM or 1st model varb 
     glevel=0
   fi
   fint="-2500;-1000;-500;-250;-100;-50;50;100;250;500;1000;2500"
   FLINE="28-23;31;20-12";;
 TCLD|CLD )
#   title=" TOTAL  _ (%) valid ? ~ "
   fint="-70;-50;-30;-20;-10;-5;5;10;20;30;50;70"
   FLINE="14-19;31;20;5;3;21-25";;
 OMEG )
   VARB="MUL(OMEG,-3600.)"
   fint="5"
   FLINE="28-23;31;20-12"
   title=" Vertical Motion (cm/s) valid ? ~ ";;
 FXSH|FXLH)
   fint="-100;-80;-60;-40;-20;-10;10;20;40;60;80;100"
   FLINE="28-23;31;20-12";;
 SOIM) 
   fint="-.5;-.4;-.3;-.2;-.1;-.05;.05;.1;.2;.3;.4;.5"
   FLINE="14-19;31;20;5;3;21-25";;
 HGHT)
   ovly=r;ovly3=r
   gdpfun=HGHT
   fint="250/250/4500"
   FLINE="31;28-10";;
 *) 
   fint="-10;-8;-6;-4;-2;-1;1;2;4;6;8;10"
   FLINE="28-23;31;20-12";;
esac      

# SET Vertical Coordinate for GEMFILE1 (NAM)
# Test here in case glevel was changed above for NAM varb
# NAM varbs (eg: TMPF,SPED...) will have different levels than smart dng varbs (usually 0)
case $glevel in
             0 ) coord=none;;
          2|10 ) coord=HGHT;;
     [11-1099] ) if [ $HYBL != H ];then coord=PRES;fi;;
  [1100-99999] ) coord=SGMA;;
     PBLR|LLTW ) coord=$glevel;glevel=0;;
            *  ) echo "HYBRID LEVEL ASSUMED" $coord;;
esac
if [ $HYBL = H ];then 
  ocord=HYBL
  ocord=`echo $coord|cut -c 2-3` 
fi
slvl=`echo $glevel|grep :`
if [ -n "$slvl" ];then coord=DPTH;fi

cint=0
# Color fill Difference plot 
if [ -z "$gdpfun" ];then
  gdpfun=" sub(${VARB} @${glevel} % ${ocord},${VARB} +2 @${glevel} % ${ocord})"
fi

gdfile2="gemfile2 + gemfile1"
if [ "$ovly" = "r" ];then
# plot Wind vector diffs top of diff fields.
 ovarb=WND
 ocord=NONE 
 oglvl=0
 otype=A
 ocint=
 oline="0;20-30/2/2/2/2/2/F"
 oclear=n
# Vector Barb Difference plot Overlaid on color fill scalar plot
   if [ -z "$gdpfun2" ];then
  gdpfun2=" vsub($ovarb @${oglvl} % ${ocord},${ovarb} +2 @${oglvl}  % ${ocord}) "
  fi
  if [ $ovly3 = r ];then  
    gdfile2="gemfile1"
    gdpfun2="kntv(wnd)"
  fi
fi

if [ $vhdrm = P0 ];then
  fint="-1;-.75;-.5;-.25;-.1;-.01;.01;.1;.25;.5;.75;1.0"
  FLINE="14-19;31;20;5;3;21-25"
fi

#===========================================================
#  CREATE MODEL DIFFERENCE PLOTS
#=========================================================== 
let plt=0
for mdl in ${mdlin[*]};do 
  inest=`echo ${reg[plt]}|awk '{ print( index($0,"nest") )}' `
  if [ $inest -gt 0 -a $mdl = smart ];then
   case ${reg[plt]} in 
        conusnest|conus   )reg[plt]=conus;;
    conusnest2p5|conus2p5 ) reg[plt]=conus2p5;;
           alaskanest|ak  ) reg[plt]=ak;;
              aknest3|ak3 ) reg[plt]=ak3;;
           hawaiinest|hi  ) reg[plt]=hi;;
           priconest|pr|pu   ) reg[plt]=pr;;
            guam|guamnest ) reg[plt]=guam;;
    * ) reg[plt]=`echo ${reg[plt]} |cut -c 1-3`;;
   esac
  fi

# Create title 
# HARDWIRE TITLE 
# Create title from dir name
  arlbl=`echo $plotarea |tr '[a-z]'  '[A-Z]'`

  mdlt=" EXP ${mdlin[0]} - CTL ${mdlin[1]} "
  title="$mdlt $arlbl @ _ valid  ~ "
  echo TITLE $title

  case $mdl in
    nam| namx| namy )
      export HOLDIN=${dirin[plt]}
      GRD=${reg[plt]} 
      gemfl[plt]=${HOLDIN}/$mdl$GRD.hiresf.gem$fhr;;
    smart )
      export HOLDIN=${dirin[plt]}
      GRD=${reg[plt]} 
      gemfl[plt]=${HOLDIN}/$mdl$GRD.gem$fhr;;
   nmm )
#    NAM  NEST file 
     skipv="/2;2"
     export HOLDIN=/com/nam/prod/nam.$yyyymmdd
     GRD=hiresf
     gemfl[plt]=${HOLDIN}/$mdl$GRD.gem$fhr;;
  esac 

  if [ -s ${gemfl[plt]} ];then
   echo "GEMFILE $plt : " ${gemfl[plt]} FOUND 
   ln -fs ${gemfl[plt]} gemfile${((plt+1))}
  else
   echo "INPUT GEMFILE FILE $plt : " ${gemfl[plt]} "NOT FOUND"
   exit
  fi
  let plt=$plt+1
done
    
#=======================================================
# plot mdl differences
echo VARB = $VARB on $coord Surfaces at level $glevel
#=======================================================
plotfile=${mdl}${gpoint}${vhdr}_DIFF${fhr}.gif

#   AQM PRECIP is accumulated to 3 hours and then dumped (P01I, P02I, P03I)
if [ $vhdrm = P0 ]; then
   apcp=$((fhr%3))
   if [ $apcp -eq 0 ];then apcp=3;fi
   VARB="P0${apcp}I"
   echo APCP $apcp VARB $VARB
 fi

#=========================================================
# Plot model Maps
#=========================================================
gemhr="${yymmdd}/${cyc}00F0${fhr}"
# gemhr="f${fhr}"

typeset -Z2 fhr1 fhr2 
if [ $avghr -gt 0 ];then
  let fhr1=$fhr-$avghr+1
  let fhr2=$avghr+$fhr1-1
  if [ $VARB = $VARBAVG ];then gemhr="F0$fhr1:F0$fhr2";fi
fi
echo VARB $VARB VARBAVG $VARBAVG $gemhr $coord $glevel 

# Negate the colors ahead of time except white and black
gpcolor>gdplotdiff$vhdr.log << EOFC
COLORS="0=255:255:255;30=109:200:191;29=0:166:81"
DEVICE   = ${dev}|${plotfile}|800;850
l
r
#COLORS="28=226:234:50;27=255:242:0;26=255:209:5"
#COLORS="25=255:153:0;24=255:0:0;23=220:28:36;22=180:39:45;21=223:115:255"
#COLORS="20=169:0:230;19=115:0:76"

ex
EOFC

#gemfile1=control=d2 dir
#gemfile2=exp=dir
gdplot2 >>gdplotdiff$vhdr.log << EOF
  GDFILE   = gemfile2+gemfile1  
  GDATTIM  = last      
  GLEVEL   = $oglvl
  GVCORD   = $ocord                       
  GDPFUN   = $gdpfun
  CINT     = $cint                           
  LINE     = $line                           
 \$MAPFIL = $mapf
  MAP      = "3/1/3 + 32/1/3"
  BORDER   = 1
  WIND     = AK32/0.8/1.2/0221/0.6 
  TITLE    = "1.5/-1/${title}"              
  DEVICE   = ${dev}|${plotfile}|800;850
  SATFIL   =
  RADFIL   =
  PROJ     = $proj
  GAREA    = $plotarea
  CLEAR    = $oclear                      
  PANEL    = .1;.1;.9;.9/1/1/3/PLOT        
  TEXT     = $TTEXT
  SCALE    = 0
#      LATLON   = 4/8/1/1/0.5;0.5
  LATLON   = 0
  HILO     = 0
  HLSYM    = 0
  CLRBAR   = $clrbar                         
  CONTUR   = 0
  FINT     = $fint                          
  FLINE    = $FLINE                        
  TYPE     = $type                        
  LUTFIL   =
  STNPLT   = $stnplt
  IJSKIP   = 
  SKIP     = $skipv
  FILTER   = 
l
r
\

  GDFILE   = ${gdfile2}
  GDPFUN   = $gdpfun2
  WIND     = AK32/0.8/1.2/0221/0.6 
  TITLE=0
  type=${otype}
l
$ovly
\

  GDFILE   = gemfile2
  WIND     = AK2/0.8/1.2/0221/0.6 
l
$ovly3
\

ex
EOF
if [ "$ovly" = "r" -a -s "$obsfile" ];then
  dattim="${obsdate}/${obshr}"
  echo OBS FILE FOUND $obsfile $dattim
  sfmap >> sfmap${vhdr}.log << EOF
   AREA     = $plotarea
   GAREA    = $plotarea
   SATFIL   =
   RADFIL   =
   IMCBAR   =
   SFPARM   = brbk:1.2:3;sknt:1.5:3
   DATTIM   = $dattim
   SFFILE   = ${obsfile}
   COLORS   = 14;4; 3; 29;22; 15;  18;  6;   1
   MAP      = 0
   MSCALE   = 
   TITLE    = 0
   CLEAR    = NO
   DEVICE   = ${dev}|${plotfile}|800;850
   PANEL    = .1;.1;.9;.9/1/1/3/PLOT        
   PROJ     = $proj
   FILTER   = NO
   TEXT     = $TTEXT
   LUTFIL   =
   STNPLT   = 0
   CLRBAR   =
save
run
\

ex
EOF
else
 echo OBS FILE NOT FOUND $obsfile $dattim
fi


gpend
exit
