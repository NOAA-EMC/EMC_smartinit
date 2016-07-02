SHELL=/bin/sh
#
###SRCS=SMARTPRECIP.f90
OBJS=SMARTPRECIP.o

FC =   ifort -convert big_endian -g -traceback	

LIBS = -L/usr/local/lib   \
	-L/nwprod/lib  -lip_4 -lw3nco_4 -lw3emc_4 -lbacio_4 -lg2_4 -lg2tmpl -lpng -lz -ljasper
NCEP_LIB_DIR = 
INC = -I/nwprod/lib/incmod/g2_4 -I/nwprod/lib/incmod/g2tmpl

FFLAGS = -O -fp-model strict -list
LDFLAGS =-FR -assume byterecl -check all -debug all -warn nounused 

CMD =	../../exec/smartprecip_g2    
PROFLIB =	-lprof

all:		$(CMD)

$(OBJS): %.o: %.f90
	$(FC) $(INC) -c $*.f90


$(CMD):		$(OBJS) 
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

clean:
	-rm -f $(OBJS)
