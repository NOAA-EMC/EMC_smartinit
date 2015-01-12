#!/bin/ksh
echo "========================================================================================="
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

if [ ${#mdlin[*]} -eq 1 ];then
clrbar="1/V/LL/.08;.1/.6;.03/-1"
fi
# TEXT size/font/width/border/rotation/justification/HW or SW
TTEXT="0.9/22/0.9/111///SW"

# LINE color/type/width/label frequency/smoothing/filter/suppress sml contours 
line="1/1/3/0/2/2/T"

mapf="HISTUS.NWS + HIPOWO.CIA "
latlon=0
skipv[plt]="/12;12"
stnplt=0
plt=0
filter=YES
filsfc=YES
cpyfil=GDS # or set grid in grdnav.tbl

echo PLOTAREA  $plotarea  GPOINT $gpoint
izoom1=`echo $plotarea|awk '{ print( index($0,"*") )}'`
izoom2=`echo $plotarea|awk '{ print( index($0,"**") )}'`
izoom3=`echo $plotarea|awk '{ print( index($0,"***") )}'`
izoom4=`echo $plotarea|awk '{ print( index($0,"****") )}'`
izoom=1
case $plotarea in pr|pu|sv|sw)izoom1=1;izoom2=1;izoom3=1;;esac

if [ $izoom1 -gt 0 ];then latlon=0;stnplt=0;skipv[plt]="/6;6";fi
if [ $izoom2 -gt 0 ];then latlon=0;stnplt=0;skipv[plt]="/3;3";fi
if [ $izoom3 -gt 0 ];then latlon="4/8/3/0.5/0.5;0.5" ;skipv[plt]="/1;1";izoom=1.5;fi
if [ $izoom4 -gt 0 ];then skipv[plt]=0;izoom=1.5;fi
stsz=$(echo "0.6*$izoom" |bc )
stwd=$(echo "0.6*$izoom" |bc )
stnplt=${stnplt:-"4/${stsz}//${stwd}/0|2|sfstns.tbl"}
if [ $izoom3 -gt 0 ];then stnplt="4/${stsz}//${stwd}/0|2|sfstns.tbl";filsfc=no;filter=no;fi
echo IZOOM $izoom1 $izoom2 $izoom3 $izoom4

# SET OVERLAY VARB--> WINDS except for precip plots which uses PMSL
gdpfunovly="kntv(wnd)"
gvcordovly=none;gvcordovly[1]=none # For smart files
glevelovly=0;glevelovly[1]=0
typeovly=b
cintovly=0

#=====END USER SETS============================================

typeset -Z2 cyc avghr vavg

# Assumed format for averaged variable in gdavg.sh
# CYCle: run cycle eg: 00, 06, 12, 18...
export cyc=$1
# FHR: forecast hour eg: from 0 to 84...
export fhr=$2
vdate="${vdate:-${yyyymmdd}${cyc}}"
export obsdate=${obsdate:-`/nwprod/util/exec/ndate $fhr $vdate |cut -c 1-8 `}
export vhr=${vhr:-`/nwprod/util/exec/ndate   $fhr $vdate |cut -c 9-10`}
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
  case $vhdr in POP0|POP1|POPZ )vhdr=$VARB;;esac
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

# settings for default gempak  obs file for comparison to model 
obsfile=${obsdate}.sfc
obsmtr=$obsfile
yymmdd=`echo $yyyymmdd | cut -c 3-8`

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
plt=0
case $vhdr in 
  TMPC )
    fint="-15;-12;-9;-6;-3;0;3;6;9;12;15;18;21;24"
    FLINE="28-14";;
  TMNK|TMXK)
    fint="250-300-5"
    FLINE="28-14";;
  TMPF) 
    sfparm="tmpf:1:1.5"  #sfc obs variables to plot
    case ${reg[plt]} in 
      ak|ak3|alaskanest|aknest3)
#SUMMER          fint="20;24;28;32;36;40;44;48;52;56;60;64;68;72;76;80;84;88";;
       fint="0;4;08;12;16;20;24;28;32;36;40;44;48;52;56;60;68";;  #Winter
      hi|hawaiinest|pr|priconest)
        fint="44;48;52;56;60;64;68;70;72;74;76;78;80;84;88;92";;
      guam|guamnmmb|guamarw)
        fint="60;64;68;70;72;74;76;78;80;82;84;86;88;90;92";;
      *)
#SUMMER        fint="44;48;52;56;60;64;68;72;76;80;84;88;92;96;100";;
        fint="8;12;16;20;24;28;32;36;40;44;48;52;56;60;64;68;72;76;80;84;88";; 
   esac
   FLINE="31-2";;
  DWPF)
    sfparm="dwpf:1:1.5"  #sfc obs variables to plot
    case ${reg[plt]} in 
      ak|ak3|alaskanest|aknest3)
  fint="0;4;08;12;16;20;24;28;32;36;40;44;48;52;56;60;68";; 
#SUMMER        fint="20;28;32;40;48;52;56;60;64;68;72;76";;
      guam|guamnmmb|guamarw|hi|hawaiinest|pr|priconest)
        fint="40;44;48;52;56;60;64;68;72;76;80;84";;
       *)
        fint="10;20;28;32;40;48;52;56;60;64;68;72;76";;
#SUMMER fint="32;40;48;52;56;60;64;68;72;76;80;84";;
    esac 
    FLINE="15-20;21;3;22-25;26-30";;
  SPFH )
    fint=".002;.004;.006;.008;.010;.012;.014;.016;.018"
    FLINE="16-20;21;3;22-25";;
  RELH|MINR|MAXR|POP03|POP06|POP12|POPZ06|C03W )
    sfparm="relh:1:1.5"  #sfc obs variables to plot
    fint="10;20;40;50;60;70;80;90;100"
    FLINE="32;16-20;21;3;22-25";;
  VSBY )
    fint="3000/0/24000"
    FLINE="24-17;32";;
  SPED|SKNT|GUST|GKNT )

#   Use special wind mesonet file
	    obsfile=mesowinds_t${CHR}z_${obsext}.sfc
	    if [ $vhdr = SKNT ];then VARB="MUL(SPED,1.944)";vlbl="SPEED (kts)";fi
	    if [ $vhdr = GKNT ];then VARB="MUL(GUST,1.944)";vlbl="GUST (kts)";fi
	    fint="5;10;15;20;25;30;35;40;45;50;55;60"
	    FLINE="32;19-20;5;21-22;25-26;28-30;15-12";;
	  DRCT )
	    fint="45"
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
	    fint="5;25;50;100;150;200;250;300;350;400;450;500"
	    FLINE="0; 26-14"
	    if [ $VARB = FXLH ];then FLINE="0; 16-30";fi;;
	  BWNR )
	    VARB="QUO(FXSH,FXLH)"
	    vlbl="Bowen Ratio"
	    fint="0.25;1;1;1.5;2.;2.5;3.0;4.0;5.0;7.5;10.;15.;20.;25."
	    FLINE="0; 26-12";;
	  SOIM)
	    fint=0.1
	    FLINE="0; 16;18;20;21-30";;
	  TRKE )
	    fint=".5;1.0;1.5;2.0;2.5;3.0;4.0;5;7.5;10"
	    FLINE="32;24-14"
	    vlbl=" TKE ( J kg**-1)" ;;
	  HGHT )
	#   Use special wind mesonet file
	    obsfile=mesowinds_t${CHR}z_${obsext}.sfc
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


#===========================================================
#  CREATE MODEL and OVERLAID OBSERVATION PLOTS 
#===========================================================
echo MODELS ${mdlin[*]};echo GRIDS ${reg[*]} 
echo VARB = $VARB on $coord Surfaces at level $glevel OVRLY= $ovrly

let plt=0
for MDL in ${mdlin[*]};do 
  inest=`echo ${reg[plt]}|awk '{ print( index($0,"nest") )}' `

# Set wind vector and obs overlay option switch (ovrly=r)
 case $vhdr in 
  SPED|SKNT|DRCT|PRES|HGHT ) 
  if [ $plotarea = dset ];then
    case ${reg[plt]} in 
    hi|hawaiinest|pr|priconest|guamnmmb|guamarw) ovrly=r;;
                                              *) ovrly=;;
    esac
  else 
    ovrly=r
  fi;;
 esac

if [ "$ovrly" != "r" ];then skipv[plt]=0;fi  # only skip if ovlaying vectors

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

  case $MDL in
#==============================================================
    nam|namx)
#==============================================================
    case ${reg[plt]} in conus ) reg[plt]=12;; esac 
    case $vhdr in 
      SPED|SKNT|DRCT ) glevel[plt]=10;coord[plt]=hght;;
      TMPF|DWPF|RELH ) glevel[plt]=2;coord[plt]=hght;;
    esac
    glevelovly[plt]=10;gvcordovly[plt]=hght  #wind ovly

#  Use nam nest to compare smartinit from 00-60
    if [ $inest -gt 0 ];then
      GRD=${reg[plt]}.hiresf # conusnest, alaskanest
      if [ $izoom1 -le 0 -a $plotarea != dset ];then skipv[plt]="/2;2";fi
    else
#     Use nam parent to compare to smartinit from 00-84
      if [ $izoom1 -gt 1 ];then skipv[plt]=0;fi
      GRD=awip${reg[plt]}    # awip12, ak, hi, but no pr ???
    fi
    export HOLDIN=${gribd[plt]}  #location of input grib file
    fhrfl=$fhr
    if [ $fhrplot -gt 0 ];then fhrfl=$fhrplot;fi   # to plot previous hr varb in same file

    export mdlfile=${MDL}.t${cyc}z.${GRD}${fhrfl}.tm00;;

#==============================================================
    dgex)
#==============================================================
    case $vhdr in 
      SPED|SKNT|DRCT ) glevel[plt]=10;coord[plt]=hght;;
      TMPF|DWPF|RELH ) glevel[plt]=2;coord[plt]=hght;;
    esac
    glevelovly[plt]=10;gvcordovly[plt]=hght  #wind ovly
    if [ $izoom1 -gt 1 ];then skipv[plt]=0;fi
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
        2|10) glevel[plt]=0;coord[plt]=none;glevelovly[plt]=0;gvcordovly[plt]=none;;
      esac
      if [ $inest -gt 0 ];then
        case ${reg[plt]} in 
          conus) reg[plt]=conus;;
          conusnest) reg[plt]=conus;;
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

    urma|urma2p5)
#===========================================
#   Special analysis plot 
#===========================================
      case $vhdr in 
        SPED|SKNT|DRCT|GUST ) glevel[plt]=10;gcoord[plt]=hght;;
        TMPF|DWPF|RELH ) glevel[plt]=2;coord[plt]=hght;;
      esac
      glevelovly[plt]=10;gvcordovly[plt]=hght  #wind ovly
      export HOLDIN=${gribd[plt]}
      GRD=2dvaranl_ndfd
      fhrv="${obsdate}/${obshr}"
      mdlfile=${MDL}.t${vhr}z.${GRD}.grb1;;

   nmm )
#==============================================================
#   NAM NEST file format
#==============================================================
    export HOLDIN=${gribd[plt]}
    GRD=bsmart
    mdlfile=nam.t${cyc}z.${reg[plt]}.${GRD}${fhr}.tm00
    skipv[plt]="/2;2";;
  esac 

# Set map file/vector skip for large domains
  case ${plotarea} in
   dset|ak|ak3|swse|sese|nwse|nese ) stnplt=;skipv[plt]="/20;20";filsfc=yes;;
       conus|conus2p5|conusnest|12 ) stnplt=;skipv[plt]="/25;25";filsfc=yes;;
                                 * )
# mapf="HIRVUS.USG + HISTUS.NWS "
# mapf="LORVWO.CIA + HISTUS.NWS "
# mapf="histus.nws+HIPOWO.CIA"
     mapf="HICNUS.NWS + HISTUS.NWS "
     latlon=${latlon:-"4/8/3/1/2;2"};;
  esac

# Create title from dir name
  gdrt=`echo ${gribd[plt]} |cut -d/ -f2`
  gdrt2=`echo ${gribd[plt]} |cut -d/ -f4`
  gdrt2=`echo $gdrt2 |tr '[a-z]'  '[A-Z]' `
  wname=`echo ${gribd[plt]} |cut -d/ -f7`
  wname=`echo $wname |tr '[a-z]'  '[A-Z]' `
  MDLT=`echo $MDL |tr '[a-z]'  '[A-Z]' `
  case $gdrt in 
        com) mdlt=" NCO $gdrt2 ${reg[plt]}";;
ptmpp1|meso) mdlt=" $gdrt2 $wname ${reg[plt]}";;
          *) mdlt=" $gdrt $gdrt2 $wname ${reg[plt]}";;
  esac
  arlbl=`echo $plotarea |tr '[a-z]'  '[A-Z]'`
  flbl=forecast
  title[plt]=" ${mdlt} ${MDLT} @ $vlbl $arlbl  ~ "
  case $MDLT in URMA|URMA2P5) title[plt]=" $gdrt ${MDLT} @ $vlbl Analysis $arlbl  ~ ";;esac
  if [ ${#mdlin[*]} -eq 1 ];then title[1]=;fi
  echo $plt   TITLE:  ${title[plt]}   SKIPV  ${skipv[plt]}

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
  if [ "$prodpl" = "com" ];then
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
  id2hdr=`echo ${work}|awk '{ print( index($0,"d2") )}'`
  if [ $id2hdr -gt 0 ];then d2hdr="d2";fi
  dsetdir=${d2hdr}${mdld[plt]}${reg[plt]}${cyc}
  if [ $plotarea != dset ];then
    if [ "$dsetdir" != "$work" ];then
    if [ -s ../${dsetdir}/${gemfile[plt]} ];then
      echo $plt DSET GEM DIR $dsetdir  FILE ${gemfile[plt]} FOUND
      ln -fs ../${dsetdir}/${gemfile[plt]} ${work}
    else 
      echo $plt DSET GEM DIR $dsetdir FILE ${gemfile[plt]} NOT FOUND
    fi
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
    echo $plt WKDIR: `pwd`
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
      if [ $plt -ge 1 ];then exit;fi
    fi
  fi
  let plt=plt+1
  cd $wkdir
done            # MDL LOOP

if [ $VARB = NULL ];then gpend; exit;fi
  

# AQM PRECIP is accumulated to 3 hours and then dumped (P01I, P02I, P03I)
  typeset -Z2 apcp
  if [ $vhdrm = P0 ]; then
    let apcp=$((fhr%3))
# only needed for cmaq files    if [ $apcp -eq 0 ];then apcp=3;fi
#   VARB="P${apcp}I"
    VARB=`echo $VARB |cut -c 1-4` 
  fi
#IF gemfile found in wkdir2, set panel to plot two plots on one graph
if [ -n "$wkdir2" ];then gemfile[1]="$wkdir2/${gemfile[1]}";else gemfile[1]=null;fi
if [ -s ${gemfile[1]} ];then
  title[1]="2.0/-1/${title[1]}"
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
  gemfile[1]=;panel2=; title[1]=;
  panel=".1;.1;9;.9/1/1/3/PLOT"
  clear=y
  echo "plotting one variable"  $VARB "one plot on one graphic " `pwd`
  echo gemfile $gemfile
fi

#=========================================================
# Plot model Maps
#=========================================================

# fhrplot flag to plot previous hour's field 
# Useful for smart files that contain previous 2 hours T/TD
if [ $fhrplot -gt 0 ];then 
  fhr=$fhrplot
  echo PLOT PREVIOUS HOUR $fhrplot VARB $VARB
fi   

# convert 00 hr file smart name to nest name
if [ $fhr -eq 00 -a $mdlin = smart ];then
  gpoint1=`echo $gpoint |cut -c 1-2`
  gpoint2=`echo $plotarea |cut -d* -f1`
  izoom1=`echo $plotarea|awk '{ print( index($0,"*") )}'`
  if [ $izoom1 -eq 0 ];then gpoint2=$plotarea;fi
   if [ $gpoint2 = dset ];then gpoint2=;fi
   case $gpoint1 in
     co)gpoint=conus2p5$gpoint2;;
     ak)gpoint=ak3$gpoint2;;
   esac
fi

MDLP=$mdlin
if [ ${mdlin[0]} != smart ];then MDLP=${mdlin[0]};fi
if [ ${mdlin[1]} != smart ];then MDLP=${mdlin[1]};fi
plotfile=${MDLP}${gpoint}${vhdr}_${fhr}.gif

gemhr="${yymmdd}/${cyc}00F0${fhr}"
gemhr="f${fhr}"

# For urma analysis, use valid time(fhrv), rather than forecast time
if [ -z "$fhrv" ];then fhrv="f${fhr}";fi

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
   GDATTIM  = "f$fhr                  ! $fhrv"
   GLEVEL   = $glevel                 ! ${glevel[1]}
   GVCORD   = $coord                  ! ${coord[1]}
   GDPFUN   = ${VARB[0]}              ! ${VARB[1]}
   CINT     = $cint 
   LINE     = $line
   \$MAPFIL = $mapf 
   MAP      = "1/2/2 + 1/1/2"        ! "1/2/2 + 1/1/2"
   BORDER   = 1
   WIND     = BK1/0.7/1.2/0221        ! BK1/0.7/1.2/0221
   TITLE    = "2.0/-1/${title[0]}"        ! ${title[1]}
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
   FILTER   = 
   SKIP     = 0
l
r

       GDFILE   = $gemfile    ! ${gemfile[1]}
       CINT     = $cintovly   ! $cintovly
       GDPFUN   = $gdpfunovly ! $gdpfunovly
       GVCORD   = $gvcordovly ! ${gvcordovly[1]} 
       GLEVEL   = $glevelovly ! ${glevelovly[1]}
       TYPE     = $typeovly   ! $typeovly
       PANEL    = $panel      ! $panel2
       SKIP     = $skipv    ! ${skipv[1]}
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
if [ -n "$sfparm" ];then
  for OBSFILE in $obsfile $obsmtr;do
    if [ -s "$OBSFILE" ];then
      if [ $izoom3 -gt 0 ];then filsfc=NO;fi
#      if [ $OBSFILE = $obsmtr ];then filsfc=NO;fi
      echo "*** PLOT OBS FILE " $sfparm  $OBSFILE $dattim $filsfc
   sfmap >> sfmap${vhdr}.log << EOF
     AREA     = dset
     GAREA    = $plotarea 
     SATFIL   =
     RADFIL   =
     IMCBAR   =
     SFPARM   = $sfparm
     DATTIM   = $dattim
     SFFILE   = ${OBSFILE} 
     COLORS   = 1;2; 3; 4;21; 15;  18;  6;   1
     MAP      = 0
     MSCALE   = 0
     TITLE    = 0     
     CLEAR    = NO
     DEVICE   = ${dev}|${plotfile}|700;700
     PANEL    = $panel      
     PROJ     = $proj
     FILTER   = $filsfc
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
      echo OBS FILE NOT FOUND $OBSFILE $dattim 
    fi
  done
fi
echo "plotmet.sh Completed : $vdate $VARB  FHR=$fhr";echo
gpend
exit
