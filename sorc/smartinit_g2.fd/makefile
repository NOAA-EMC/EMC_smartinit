SHELL=/bin/sh
#
#SRCS=    aset3d.f aset2d.f grddef.f constants.f asetdown.f rdgrib.f     \
GETGRIB.f90 MAIN.f90 BOUND.f90 GRIBIT.f90 GET_BITS.f90  MAKESTRING.f90 NDFDgrid.f90 VADJUST.f90 w3fi71.f w3fi63.f

OBJS1= aset3d.o aset2d.o grddef.o constants.o asetdown.o rdgrib.o 
OBJS2= GETGRIB.o MAIN.o BOUND.o GRIBIT.o GET_BITS.o MAKESTRING.o NDFDgrid.o  VADJUST.o 
OBJS3= w3fi71.o w3fi63.o

## FC =   ifort -convert big_endian -g -traceback -assume noold_ldout_format -O -debug all -fp-model strict
###FC =   ifort -convert big_endian -g -traceback -assume noold_ldout_format
FC =   ifort -convert big_endian -g -traceback 
#FC =   ifort -convert big_endian -g -traceback -assume noold_ldout_format -debug all -fp-model strict -ftrapuv -check all -fp-stack-check -fstack-protector

LIBS = -L/usr/local/lib   \
    -L/nwprod/lib  -lip_4 -lw3nco_4 -lw3emc_4 -lbacio_4 -lg2tmpl -lg2_4 -ljasper -lpng -lz
NCEP_LIB_DIR = 
INC = -I/nwprod/lib/incmod/g2_4 -I/nwprod/lib/incmod/g2tmpl

# FFLAGS = -O -fp-model strict -list 
# LDFLAGS =-FR -assume byterecl -check all -debug all -warn nounused  

FFLAGS = -O2   ## -debug all  ## -fp-model strict -list 
###FFLAGS = -O -fp-model strict -list
#LDFLAGS =-FR -assume byterecl # -check all -debug all -warn nounused  
LDFLAGS =-FR -assume byterecl # -check all -debug all -warn nounused

CMD =	../../exec/smartinit_g2_rw

PROFLIB =	-lprof

all:		$(CMD)

$(OBJS1): %.o: %.f90
	$(FC) $(INC) -c $*.f90

$(OBJS2): %.o: %.f90
	$(FC) $(INC) -c $*.f90

$(OBJS3): %.o: %.f
	$(FC) $(INC) -c $*.f

$(CMD):		$(OBJS1) $(OBJS2) $(OBJS3)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(@) $(OBJS1) $(OBJS2) $(OBJS3) $(LIBS)

clean:
	-rm -f $(OBJS1) $(OBJS2) $(OBJS3) *.mod
