#!/bin/ksh
echo "======================================================================="
echo into PLOTMET.SH : cyc $1 fhr $2 varb $3 lvl $4 plotarea $5 reg ${reg[*]}  DATE ${yyyymmdd}

#####################################################################
#  Script to produce NAWIPS Surface maps and  overlay obs if available
#  Input files:
#     NAM p-grib file
#     or
#     NAM GEMPAK pressure grib file: nam.gem
#     and 
#     Surface GEMPAK observations file if available: sfc[YYYYMMDD].gem
#####################################################################

dev=GIF
plottype=full
# rootd defined in parent job
export diagdir=$rootd/gddiag

# MDL to plot: eg: nam, smart, aqm, obs
set -A gribd ${gribd} ${gribd2} #grib file dir (/com/$mdl/prod...)
set -A mdlin ${mdlin}           #mdl (nam,smart,dgex, gfs,hiresw)
set -A mdld ${mdld[*]}          #root dir (nam, dgex,gfs, hiresw)
set -A reg ${reg[*]}            #mdl run domain (conus, conus2p5, hi,pr, ak3...)

#===========================================================
#=SET GRAPHICS DEFAULTS based on area: line, text, mapfile...
#===========================================================

if [ -z "$plotarea" ];then export plotarea=dset;fi
if [ -z "$proj" ];then export proj=;fi

# SET label for plot file name (gpoint)
#TEST gpoint=`echo $reg | cut -c 1-3`

# COLOR BAR: color/V/Location/Starting x;y/box length;width/label location
clrbar="1/V/UL/.08;.5/.9;.03/-1"

# TEXT size/font/width/border/rotation/justification/HW or SW
TTEXT="0.9/22/0.9/111///SW"

# LINE color/type/width/label frequency/smoothing/filter/suppress sml contours 
line="1/1/3/0/2/2/T"
mapf="HISTUS.NWS + HIPOWO.CIA "
skipv="/2;2"
filter=YES
cpyfil=GDS # or set grid in grdnav.tbl

echo plotarea  $plotarea gpoint $gpoint
izoom1=`echo $plotarea|awk '{ print( index($0,"*") )}'`
izoom2=`echo $plotarea|awk '{ print( index($0,"**") )}'`
izoom3=`echo $plotarea|awk '{ print( index($0,"***") )}'`
izoom4=`echo $plotarea|awk '{ print( index($0,"****") )}'`
izoom=1
if [ $izoom2 -gt 0 ];then latlon="4/8/3/1/1;1" ;fi
if [ $izoom3 -gt 0 ];then latlon="4/8/3/0.5/0.5;0.5" ;skipv="/1;1";izoom=1.5;fi
if [ $izoom4 -gt 0 ];then skipv=0;izoom=1.5;fi
stsz=$(echo "0.6*$izoom" |bc )
stwd=$(echo "0.6*$izoom" |bc )
stnplt="1/${stsz}//${stwd}/0|3|sfstns.tbl"
echo IZOOM $izoom $izoom2 $izoom3 $stsz $stwd

case ${gpoint} in
                  dset|ak|ak3 ) stnplt=;skipv="/20;20";;
  conus|conus2p5|conusnest|12 ) stnplt=;skipv="/25;25";;
                            * )
#   mapf="HICNUS.NWS + HISTUS.NWS "
#   mapf="HIRVUS.USG + HISTUS.NWS "
    mapf="LORVWO.CIA + HISTUS.NWS "
    filter=NO
    latlon="4/8/3/1/2;2";;
esac
skipv2=$skipv


# SET OVERLAY VARB--> WINDS except for precip plots which uses PMSL
gdpfunovly="kntv(wnd)"
gvcordovly=none   # For smart files
glevelovly=0
typeovly=b
cintovly=0

#=====END USER SETS============================================

typeset -Z2 cyc avghr vavg

# Assumed format for averaged variable in gdavg.sh
# CYCle: run cycle eg: 00, 06, 12, 18...
export cyc=$1
# FHR: forecast hour eg: from 0 to 84...
export fhr=$2
vdate="${yyyymmdd}${cyc}"
export obsdate=`/nwprod/util/exec/ndate  $fhr $vdate |cut -c 3-8 `
export obshr=`/nwprod/util/exec/ndate  $fhr $vdate |cut -c 9-10`
export obshr="${obshr}00"
echo OBSDATE $obsdate $obshr

# GEMPAK VARIABLE NAME: eg: tmpc, dwpc, p06i,
export VARB=$3  
vhdrm=`echo $VARB |cut -c1-2`
if [ $vhdrm = "P0" ];then 
  export vhdr=$VARB;vavg=;
else
  export vhdr=`echo $VARB |cut -c1-4` 
  vavg=`echo $VARB |grep 0`              # 01
fi 

# Vertical glevel
set -A glevel $4
export glevel

HYBL=`echo $glevel |cut -c 1-1`
if [ $HYBL = H ];then glevel=`echo $glevel|cut  -c 2-3`;fi

if [ $# -lt 4 ];then 
  clear
  echo USAGE: $0 cyc fhr varb model glevel 
  echo "========================================" 
  echo Run Cycle: 00,06,12,18      $1 
  echo Fcst Hr  : 01-48            $2 
  echo Variable : TMPC,SPED,P06I.. $3 
  echo Vert. lvl: 0,2, 10, 1000, 850...,H02,H03.. $5
  echo "========================================" 
  exit 
fi
#Set glevels for all models to plot
nplts=${#mdlin[*]}
let plt=0
while [ $plt -lt $nplts ];do
  VARB[plt]=$VARB
  glevel[plt]=$glevel
  coord[plt]=$coord
  let plt=plt+1
done

export vhdravg=`echo $VARB |cut -dA -f1`  #eg PM2501
VARBAVG=${vhdravg}A${avghr}               #eg PM2501A24 

if [ -z "$yyyymmdd" ];then 
  export yyyymmdd=`/nwprod/util/exec/ndate |cut -c 1-8`
  export avghr=0
fi
yymmdd=`echo $yyyymmdd | cut -c 3-8`
echo DATE to PLOT: $yyyymmdd

tbld=/nwprod/gempak/fix
gemtbls="${tbld}/namsminit_wmogrib2.tbl; ${tbld}/namsminit_ncepgrib129.tbl;\ 
${tbld}/vctdgrib1.tbl"
type=F    # contour type line of fill
dir=`pwd |cut -d/ -f5`     # Create title from subdir name
rootdir=`pwd |cut -d/ -f2` # Create title from subdir name
vlbl="  _  "               # Default variable label
sfparm="brbk:1.2:3;sknt:0.8:1"  #sfc obs variables to plot

#======================================================
# Set contour intervals
#======================================================
case $vhdr in 
  TMPC )
    fint="-15;-12;-9;-6;-3;0;3;6;9;12;15;18;21;24"
    FLINE="28-14";;
  TMNK|TMXK)
    fint="250-300-5"
    FLINE="28-14";;
  TMPF) 
    sfparm="tmpf:1:3"  #sfc obs variables to plot
    case $reg in 
      ak|ak3|alaskanest|aknest3)
#SUMMER fint="20;24;28;32;36;40;44;48;52;56;60;64;68;72;76;80;84;88";;
        fint="0;4;08;12;16;20;24;28;32;36;40;44;48;52;56;60;68";;  #Winter
      hi|hawaiinest|pr|priconest)
        fint="44;48;52;56;60;64;68;70;72;74;76;78;80;84;88;92";;
      guam|guamnmmb|guamarw)
        fint="60;64;68;70;72;74;76;78;80;82;84;86;88;90;92";;
      *)
#SUMMER fint="48;56;60;64;68;72;76;80;84;88;92;96;100;104";;
        fint="8;12;16;20;24;28;32;36;40;44;48;52;56;60;64;68;72;76;80;84;88";; 
   esac
   FLINE="28-10";;
  DWPF)
    sfparm="dwpf:1:3"  #sfc obs variables to plot
    case $reg in 
      ak|ak3|alaskanest|aknest3)
#summer fint="20;24;28;32;36;40;44;48;52;56;60;64;68;72;76";;
        fint="0;4;08;12;16;20;24;28;32;36;40;44;48;52;56;60;68";; 
      guam|guamnmmb|guamarw|hi|hawaiinest|pr|priconest)
        fint="40;46;50;54;56;60;64;68;72;76;80";;
       *)
#SUMMER fint="50;54;56;60;64;68;72;76;80;84;88"
        fint="20;24;28;32;36;40;44;48;52;56;60;64;68;72;76";;
    esac 
    FLINE="16-20;21;3;22-25";;
  SPFH )
    fint=".002;.004;.006;.008;.010;.012;.014;.016;.018"
    FLINE="16-20;21;3;22-25";;
  RELH|MINR|MAXR|POP0|C03W )
    sfparm="relh:1:3"  #sfc obs variables to plot
    fint="10;20;40;50;60;70;80;90;100"
    FLINE="32;16-20;21;3;22-25";;
  VSBY )
    fint="3000/0/24000"
    FLINE="24-17;32";;
  SPED|SKNT|GUST )
    if [ $vhdr = SKNT ];then VARB="MUL(SPED,1.944)";vlbl="SPEED (kts)";fi
    fint="2.5"
    FLINE="32;18-30;7";;
  DRCT )
    fint="45/45/360"
    FLINE="27-10";;
  HAIN )
    fint="0;1;2;2.5;3;3.5;4;4.5;5;5.5;6"
    FLINE="32;24-12";;
  NMXL )
   fint="1;2;3"
   FLINE="0;3;5;2;7";;
  ZPBL|DIST)
    if [ ${mdlin[0]} = nam ];then 
      VARB[0]=ZPBL
      coord[0]=none  #Assume 2nd varb is smart dng product
      glevel[0]=0
    fi
    fint="100;250;500;1000;1500;2000;2500;3000;3500;4000;4500;5000;7500;10000"
    FLINE="0;28-9"
    vlbl="PBL HGT (m)";;
  TCLD|CLD )
    fint="10;25;37.5;50;67.5;75;90;100"
    FLINE="32;16-20;21;3;22-25"
    vlbl=" TOTAL Cloud frac";;
  OMEG )
    VARB="MUL(OMEG,-3600.)"
    fint="-25;-15;0;10;20;30;40;50;75;100;250"
    FLINE="32;24-14"
    vlbl=" Vertical Motion (cm/s)";;
  SEXC )
    fint=".1;.2;.3;4;.5;.75;1.0;1.25;1.50;2.0;2.5"
    fint=0.01
    FLINE="0;24-14";;
  FXSH | FXLH )
    fint="50;100;150;200;250;300;350;400;450;500"
    FLINE="32; 24-14"
    if [ $VARB = FXLH ];then FLINE="0; 16-24";fi;;
  TRKE )
    fint=".5;1.0;1.5;2.0;2.5;3.0;4.0;5;7.5;10"
    FLINE="32;24-14"
    vlbl=" TKE ( J kg**-1)" ;;
  HGHT )
#   Hardwire for sfc topo plot
    fint="250/250/4500"
    FLINE="32;28-10;"
    vlbl=" _ (m) ";;
  PRES )
    fint="25/600/1050"
    FLINE="0;28-10;"
    vlbl=" _ (mb) ";;
  WXTR )
    fint=3
    FLINE="0;31-01;";; 
  * )
    cint=0
    fint=0
    FLINE="0;28-01";;
esac
VARB[1]=${VARB}

# Set wind vector and obs overlay option switch (ovrly=r)
case $vhdr in TMPF|DWPF|SPED|SKNT|DRCT|PRES|HGHT ) 
  if [ $plotarea = dset ];then
    case $reg in 
      hi|hawaiinest|pr|priconest|guamnmmb|guamarw) ovrly=r;;
                                                *) ovrly=;;
    esac
  else
    ovrly=r
  fi;;
esac

# Overlay PMSL on top of any precip plot
if [ $vhdrm = P0 ];then
  ovrly=r
  vavg=;
  fint=".01;.1;.25;.5;.75;1.0"
  FLINE="32;25-21;28-30"
  gdpfunovly=EMSL 
  gvcordovly=none
  glevelovly=0
  typeovly=C
  cintovly=0
elif [ $vhdrm = SN ];then
  fint=".1;1;2.5;5;7.5;10"
  FLINE="32;25-21;28-30"
  export coord=none
fi
if [ "$ovrly" != "r" ];then skipv=0;fi  # only skip if ovlaying vectors


#===========================================================
#  CREATE MODEL and OVERLAID OBSERVATION PLOTS 
#===========================================================
echo MODELS ${mdlin[*]};echo GRIDS ${reg[*]} 
echo VARB = $VARB on $coord Surfaces at level $glevel OVRLY= $ovrly

let plt=0
for MDL in ${mdlin[*]};do 
  inest=`echo ${reg[plt]}|awk '{ print( index($0,"nest") )}' `

# Set Vertical coordinate based on glevel
  case ${glevel[plt]} in 
             0 ) coord[plt]=none;;
          2|10 ) coord[plt]=HGHT;;
     [11-1099] ) if [ $HYBL != H ];then coord[plt]=PRES;fi;;
  [1100-99999] ) coord[plt]=SGMA;;
     PBLR|LLTW ) coord[plt]=${glevel[plt]};glevel[plt]=0;;
            *  ) echo "HYBRID LEVEL ASSUMED" ${glevel[plt]};;
  esac
  if [ $HYBL = H ];then coord=HYBL;fi
  slvl=`echo ${glevel[plt]} |grep :`
  if [ -n "$slvl" ];then coord[plt]=DPTH;fi

echo Running plotmet for Cycle $cyc $yyyymmdd
  case $MDL in
#==============================================================
    nam|namx)
#==============================================================
    case ${reg[plt]} in conus ) reg[plt]=12;; esac 
    case $vhdr in 
      SPED|SKNT|DRCT|GUST ) glevel[plt]=10;coord[plt]=hght;;
      TMPF|DWPF|RELH ) glevel[plt]=2;coord[plt]=hght;;
    esac
    if [ $inest -gt 0 ];then
#     Use nam nest to compare smartinit from 00-60
      GRD=${reg[plt]}.hiresf # conusnest, alaskanest
    else
#     Can use nam parent to compare to smartinit from 00-84
      GRD=awip${reg[plt]}    # awip12, ak, hi, but no pr ???
    fi
    export HOLDIN=${gribd[plt]}  #location of input grib file
    mdlfile=${MDL}.t${cyc}z.${GRD}${fhr}.tm00;;

#==============================================================
    dgex)
#==============================================================
    case $vhdr in 
      SPED|SKNT|DRCT|GUST ) glevel[plt]=10;coord[plt]=hght;;
      TMPF|DWPF|RELH ) glevel[plt]=2;coord[plt]=hght;;
    esac
    if [ $izoom -gt 1 ];then skipv2=0;fi
    case ${reg[plt]} in 
      conus ) GRD=awp185;;
      alaska) GRD=awp186;;
    esac
#    GRD=bsmart   # ERic Rogers para bsmart files
#    cpyfil="#255" # bsmart grid 255 defined in grdnav.tbl

    export HOLDIN=${gribd[plt]}  #location of input grib file
    mdlfile=${MDL}_${reg[plt]}.t${cyc}z.${GRD}${fhr}.tm00;;
#  indexfile=${mdlfile}i  #to read bsmart native grid

    smart|hiresw|gfs )
#==============================================================
#     smartinit files set temps, winds, rh at level 0 
#==============================================================
      case ${glevel[plt]} in
        2|10) glevel[plt]=0;coord[plt]=none;;
      esac
      if [ $inest -gt 0 ];then
        case ${reg[plt]} in 
          conus|conusnest )reg[plt]=conus;;
          conusnest2p5) reg[plt]=conus2p5;;
          ak_rtmages ) reg[plt]=ak_rtmages;;
          alaskanest ) reg[plt]=ak;;
             aknest3 ) reg[plt]=ak3;;
          hawaiinest ) reg[plt]=hi;;
          priconest  ) reg[plt]=pr;;
                   * ) reg[plt]=`echo ${reg[plt]} |cut -c 1-3`;;
        esac 
      fi

    export HOLDIN=${gribd[plt]}  #location of input grib file
    GRD=${reg[plt]}

#   00-60: ak or aknest3,conus or conus2p5, hi, pr
#   60-84: ak,conus,hi,pr

    mdlfile=${mdld[plt]}.t${cyc}z.smart${GRD}${fhr}.tm00;;

   nmm )
#==============================================================
#   NAM NEST file format
#==============================================================
    export HOLDIN=${gribd[plt]}
    GRD=bsmart
    mdlfile=nam.t${cyc}z.${reg[plt]}.${GRD}${fhr}.tm00
    skipv="/2;2";;
  esac 

# settings for gempak  obs file for comparison to model 
  obsfile=${yyyymmdd}.sfc

# Create title from dir name
  echo;echo $plt   GRIBD:  ${gribd[plt]}
  gdrt=`echo ${gribd[plt]} |cut -d/ -f2`
  wname=`echo ${gribd[plt]} |cut -d/ -f7`
  wname=`echo $wname |tr '[a-z]'  '[A-Z]' `
  wname2=`echo $wname2 |tr '[a-z]'  '[A-Z]' `
  mdlt=" $gdrt $wname2" 
  MDLT=`echo $MDL |tr '[a-z]'  '[A-Z]' `
  case $gdrt in 
       com) mdlt=" PROD $wname ${reg[plt]}";;
    com_p6) mdlt=" CCS $wname";;
      ptmp) mdlt=" EMC PARA ${reg[plt]}";;
         *) mdlt=" $gdrt2 ${reg[plt]}";;
  esac
  arlbl=`echo $plotarea |tr '[a-z]'  '[A-Z]'`
  title[plt]=" ${mdlt} ${MDLT} @ $vlbl forecast $arlbl  ~ "
  echo $plt   TITLE:  ${title[plt]}

#=======================================================
# plot mdl and obs surface map for a forecast hour
#=======================================================

# GEMFILE formatted Input FILE
  if [ ${mdlin[1]} = smart ];then MDL=smart;fi
  export gemfile[plt]=${mdlin[plt]}${GRD}.gem${fhr}

  work=$wkdir
  if [  $plt -ge 1 ];then work=${wkdir2};fi
  cd $work  

# Use Production GEM file if available
  prodpl=`echo ${gribd[plt]} |cut -d/ -f2`
  if [ $prodpl = "com" ];then
    prodgemd=/com/nawips/prod/${mdlin[plt]}.${yyyymmdd}
    prodgemf=${mdlin[plt]}_${reg[plt]}_${yyyymmdd}${cyc}f0${fhr}
    if [ -s ${prodgemd}/${prodgemf} ];then
      echo "PROD GEM FILE $plt FOUND:" ${prodgemd} ${prodgemf}
      cp -p ${prodgemd}/${prodgemf} $work/${gemfile[plt]}
    else
      echo "PROD GEM FILE $plt NOT AVAILABLE:" ${prodgemd} ${prodgemf}
    fi
  fi  

# When plotting several domains (plotareas), use dset GEM file if created
  d2hdr=;
  if [ $plt -ge 1 ];then d2hdr=d2;fi
  dsetdir=${d2hdr}${mdld[plt]}${reg[plt]}${cyc}
  if [ $plotarea != dset ];then
    if [ -s ../${dsetdir}/${gemfile[plt]} ];then
      echo $plt DSET GEM DIR $dsetdir  FILE ${gemfile[plt]} FOUND
      ln -fs ../${dsetdir}/${gemfile[plt]} ${work}
    else 
      echo $plt DSET GEM DIR $dsetdir FILE ${gemfile[plt]} NOT FOUND
    fi
  fi

#=========================================================
#   Convert Model Grib File to Gempak File
#=========================================================
  export GEMFL=${gemfile[plt]}
  if [ -s "$GEMFL" ];then
    echo GEMFILE : $plt $GEMFL FOUND 
  else 
#   cr8gemfl.sh $plt $WKDIR  $HOLDIN $mdlfile $GEMFL
    echo  WKDIR: $plt `pwd`
    echo "GEMFILE :"  $plt  $GEMFL Does not exist. Creating it
    if [ -s ${HOLDIN}/${mdlfile} ];then
      echo "INPUT GRIB FILE:"  $plt  $HOLDIN/$mdlfile  FOUND
      rm -f $GEMFL
      nagrib>>nagrib$vhdr.log << EOF
       GBFILE="${HOLDIN}/${mdlfile}"
       INDXFL=$indexfile
       GDOUTF=$GEMFL
       PROJ=
       GRDAREA=dset
       KXKY=   10;10
       MAXGRD= 1000
       CPYFIL= $cpyfil
       GAREA=  dset
       OUTPUT="F/${mdlfile}.txt"
       GBTBLS=
       GBDIAG=
       PDSEXT=NO
save nagrib
r
\
 
ex
EOF
    else
      echo "INPUT GRIB FILE" $plt  $HOLDIN/$mdlfile "NOT FOUND"
      gpend
      exit
    fi
  fi
  let plt=plt+1
  cd $wkdir
done            # MDL LOOP


# AQM PRECIP is accumulated to 3 hours and then dumped (P01I, P02I, P03I)
  typeset -Z2 apcp
  if [ $vhdrm = P0 ]; then
    let apcp=$((fhr%3))
# only needed for cmaq files    if [ $apcp -eq 0 ];then apcp=3;fi
#   VARB="P${apcp}I"
    VARB=`echo $VARB |cut -c 1-4` 
    echo APCP $apcp VARB $VARB
  fi
#IF gemfile found in wkdir2, set panel to plot two plots on one graph
gemfile[1]="$wkdir2/${gemfile[1]}"
if [ -s ${gemfile[1]} ];then
  panel="0;0;1;.49/1/1/3/PLOT"
  panel2="0;.51;1;1/1/1/3/PLOT"
  clear=n
  dir2=`echo ${wkdir2} |cut -d/ -f5`
  echo;echo "plotting variable"  $VARB "two plots on one graphic " `pwd`
  echo gemfile1 ${gemfile[0]}
  echo gemfile2 ${gemfile[1]}
else
  echo "*****************************************************"
  echo "WARNING 2nd GEMFILE NOT FOUND" ${gemfile[1]}
  echo "*****************************************************"
  gemfile[1]=;panel2=; 
  panel=".1;.1;9;.9/1/1/3/PLOT"
  clear=y
  echo "plotting one variable"  $VARB "one plot on one graphic " `pwd`
  echo gemfile $gemfile
fi

#=========================================================
# Plot model Maps
#=========================================================
plotfile=${mdlin}${gpoint}${vhdr}_${fhr}.gif

gemhr="${yymmdd}/${cyc}00F0${fhr}"
gemhr="f${fhr}"
if [ $avghr -gt 0 ];then
 typeset -Z2 fhr1 fhr2 
 let fhr1=$fhr-$avghr+1
 let fhr2=$avghr+$fhr1-1
 if [ $VARB = $VARBAVG ];then
   gemhr="F0$fhr1:F0$fhr2"
 else
# Create Avged VARB and add to gempak file for plotting
   if [ $fhr -ge $avghr ];then
     echo "*** Creating $avghr h Avg for $VARB $fhr $fhr1 $fhr2 *** "
     ${diagdir}/gdavg.sh $VARB $fhr1 $fhr2 >gdplot$vhdr.log
   fi
 fi
fi

# Negate the colors ahead of time except white and black
gpcolor>gdplot$vhdr.log << EOFC
   COLORS="0=255:255:255;31=0:173:220;30=109:200:191;29=0:166:81"
   DEVICE   = ${dev}|${plotfile}|700;700
l
r
ex
EOFC

gdplot2 >>gdplot$vhdr.log << EOF
   GDFILE   = $gemfile                ! ${gemfile[1]}
   GDATTIM  = last                   
   GLEVEL   = $glevel                 ! ${glevel[1]}
   GVCORD   = $coord                  ! ${coord[1]}
   GDPFUN   = ${VARB[0]}              ! ${VARB[1]}
   CINT     = $cint 
   LINE     = $line
   \$MAPFIL = $mapf
   MAP      = "3/1/3 + 1/1/3"
   BORDER   = 1
   WIND     = BK1/0.7/1.2/0221        ! BK1/0.7/1.2/0221
   TITLE    = "2.0/-1/${title[0]}"        ! "2.0/-1/${title[1]}"
   DEVICE   = ${dev}|${plotfile}|700;700
   SATFIL   =
   RADFIL   =
   PROJ     = $proj
   GAREA    = $plotarea
   CLEAR    = $clear                   ! $clear
   PANEL    = $panel                   ! $panel2
   TEXT     = $TTEXT
   SCALE    = 0
   LATLON   = $latlon
   HILO     = 0
   HLSYM    = 0
   CLRBAR   = 0 ! $clrbar                  
   CONTUR   = 0
   FINT     = $fint
   FLINE    = $FLINE
   TYPE     = $type                   
   LUTFIL   =
   STNPLT   = $stnplt
   IJSKIP   = 
   SKIP     = $skipv    ! $skipv2
   FILTER   = 
l
r

       GDFILE   = $gemfile    ! ${gemfile[1]}
       CINT     = $cintovly   ! $cintovly
       GDPFUN   = $gdpfunovly ! $gdpfunovly
#       GVCORD   = $gvcordovly ! $gvcordovly
#       GLEVEL   = $glevelovly ! $glevelovly
       TYPE     = $typeovly   ! $typeovly
       PANEL    = $panel      ! $panel2
       STREAM   =
       FILTER   = NO
       IJSKIP   = NO
       MAP      = 0
       TITLE    = 0
       CLRBAR   = 0
       CLEAR    = n
l
$ovrly
\

\

ex
EOF

#=========================================================
# Plot observations if available
# Script assumes obs gem file already created w/ mesonet2gem.f
# and SFCFILE and SFEDIT
#=========================================================
#   SFPARM   = brbk:1:3;tmpc;dwpc;p03m*100
#   LATLON   = 4/8/2/1/4;4

dattim="${obsdate}/${obshr}"
if [ "$ovrly" = "r" -a -s "$obsfile" ];then
  echo OBS FILE FOUND $obsfile $dattim 
  sfmap >> sfmap${vhdr}.log << EOF
   AREA     = dset
   GAREA    = $plotarea 
   SATFIL   =
   RADFIL   =
   IMCBAR   =
   SFPARM   = $sfparm
   DATTIM   = $dattim
   SFFILE   = ${obsfile} 
   COLORS   = 14;2; 3; 4;22; 15;  18;  6;   1
   MAP      = 0
   MSCALE   = 0
   TITLE    = 0     
   CLEAR    = NO
   DEVICE   = ${dev}|${plotfile}|700;700
   PANEL    = $panel      
   PROJ     = $proj
   FILTER   = YES
   TEXT     = $TTEXT
   LUTFIL   =
   STNPLT   = $stnplt
   CLRBAR   =

save
run
\

  PANEL=$panel2
  r
  \

ex
EOF
else
  if [ "$ovrly" = "r" ];then echo OBS FILE NOT FOUND $obsfile $dattim ;fi
fi

gpend
exit
