SHELL=/bin/sh
#
OBJS=SMARTPRECIP.o

FC =   ifort -convert big_endian -g -traceback 

LIBS = ${G2TMPL_LIB} ${G2_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB} \
       ${IP_LIB4} ${W3NCO_LIB4} ${W3EMC_LIB4} ${BACIO_LIB4}

INCG2=-I${G2_INC4} -I${G2TMPL_INC}

NCEP_LIB_DIR =

FFLAGS = -O -fp-model strict -list
LDFLAGS =-FR -assume byterecl # -check -check all -check bounds -g -debug all -warn nounused -warn interface

CMD =	smartprecip_g2    
PROFLIB =	-lprof

all:		$(CMD)

$(OBJS): %.o: %.f90
	$(FC) $(INCG2) -c $*.f90

$(CMD):		$(OBJS) 
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

clean:
	-rm -f $(OBJS)
