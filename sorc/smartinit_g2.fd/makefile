SHELL=/bin/sh
#

OBJS1= aset3d.o aset2d.o grddef.o constants.o asetdown.o rdgrib.o 
OBJS2= GETGRIB.o MAIN.o BOUND.o GRIBIT.o GET_BITS.o MAKESTRING.o NDFDgrid.o  VADJUST.o 
OBJS3= w3fi71.o w3fi63.o

FC =   ifort -convert big_endian -g -traceback 

LIBS = ${G2TMPL_LIB} ${G2_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB} \
       ${IP_LIB4} ${W3NCO_LIB4} ${W3EMC_LIB4} ${BACIO_LIB4}

INCG2=-I${G2_INC4} -I${G2TMPL_INC}

NCEP_LIB_DIR =

FFLAGS = -O2   ## -debug all  ## -fp-model strict -list
LDFLAGS =-FR -assume byterecl # -check bounds -g # -check all -debug all -warn nounused  

CMD =	../../exec/smartinit_g2_rw

PROFLIB =	-lprof

all:		$(CMD)

$(OBJS1): %.o: %.f90
	$(FC) $(INCG2) -c $*.f90

$(OBJS2): %.o: %.f90
	$(FC) $(INCG2) -c $*.f90

$(OBJS3): %.o: %.f
	$(FC) $(INCG2) -c $*.f

$(CMD):		$(OBJS1) $(OBJS2) $(OBJS3)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(@) $(OBJS1) $(OBJS2) $(OBJS3) $(LIBS)

clean:
	-rm -f $(OBJS1) $(OBJS2) $(OBJS3) *.mod
