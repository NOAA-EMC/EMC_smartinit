SHELL=/bin/sh
#
# This makefile was produced by /usr/bin/fmgen at 10:56:31 AM on 11/10/98
# If it is invoked by the command line
#	make -f makefile
# it will compile the fortran modules indicated by SRCS into the object
# modules indicated by OBJS and produce an executable named a.out.
#
# If it is invoked by the command line
#	make -f makefile a.out.prof
# it will compile the fortran modules indicated by SRCS into the object
# modules indicated by OBJS and produce an executable which profiles
# named a.out.prof.
#
# To remove all the objects but leave the executables use the command line
#	make -f makefile clean
#
# To remove everything but the source files use the command line
#	make -f makefile clobber
#
# To remove the source files created by /usr/bin/fmgen and this makefile
# use the command line
#	make -f makefile void
#
# The parameters SRCS and OBJS should not need to be changed.  If, however,
# you need to add a new module add the name of the source module to the
# SRCS parameter and add the name of the resulting object file to the OBJS
# parameter.  The new modules are not limited to fortran, but may be C, YACC,
# LEX, or CAL.  An explicit rule will need to be added for PASCAL modules.
#
SRCS=   MAKEPRECIP.f
OBJS=   MAKEPRECIP.o	
# Tunable parameters
#
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
# PROFLIB	Library needed for profiling
#
FC =	        ifort -convert big_endian -g -traceback
LDFLAGS = 

#CCS LIBS =          -L/usr/local/lib -lessl -lmass \
                 -L/nwprod/lib -lw3_4 -lbacio_4

#LIBS =          -L/u/Ratko.Vasic/lib_tmp \
#                -lip_4 -lw3-1.9_4 -lsp_noessl_4 -lbacio_4 \
#                -L/u/Eric.Rogers/nwprod/lib \
#                -lgem

#
LIBS =          -L /nwprod/lib  -lip_4 -lw3_4 -lbacio_4\
                -L/u/Eric.Rogers/nwprod/lib -lgem 

CMD =	        nam_smartmakeprecip
PROFLIB =	

# To perform the default compilation, use the first line
# To compile with flowtracing turned on, use the second line
# To compile giving profile additonal information, use the third line
# WARNING:  SIMULTANEOUSLY PROFILING AND FLOWTRACING IS NOT RECOMMENDED 
#CCS FFLAGS =  -O   -qstrict -qddim -qrealsize=4 -qmaxmem=-1 -qsource -qxref=full -qattr -qnosave -qspillsize=548 -bloadmap:lm
FFLAGS =  -O   
#FFLAGS =         -Ovector3	
#FFLAGS =	 -F
#FFLAGS =	 -Wf"-ez"

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

# Make the profiled version of the command and call it a.out.prof
#
$(CMD).prof:	$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(PROFLIB) $(LIBS)

clean:
	-rm -f $(OBJS)

clobber:	clean
	-rm -f $(CMD) $(CMD).prof

void:	clobber
	-rm -f $(SRCS) makefile
