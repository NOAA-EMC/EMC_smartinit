#!/bin/sh
#
#  This makefile is to compile ensembel production generator
#  Author: Binbin Zhou
#          Aug 1, 2005
#  OS: AIX (IBM-blue)
#  Oct. 2, 2012: Modified for Linux OS (Jun Du)
#

SHELL=/bin/sh
FC =     ifort -O3 -convert big_endian
ARCH     = 
OPTS     = 
LIST     =
#FREE     = -qfree
FREE     =
TRAPS    =
PROFILE  =
DEBUG    = -g
MEM      =
MAP      =
W3LIBDIR = /nwprod/lib
ESSL     = 
MASS     = 


SRC=	util.f \
	char_process.f \
	print.f pack.f \
	thickness.f preciptype.f apcp.f snow.f wind.f \
	dtra.f frzn.f vorticity.f getflight.f getcldtop.f \
	mxp.f get_mxp.f pack_mxp.f \
	get_new_fog.f new_fog.f \
	get_firewx.f llws.f \
	get_cptp_severe.f  calibrate2_hrly_rgn3.f calibrate2_dryt.f  calibrate2_svr.f get_prev_precip_temp.f

OBJ=${SRC:.f=.o}

LIBS= $(ESSL) $(MASS) $(OPTS) $(LIST) $(TRAPS) $(PROFILE) $(DEBUG) -L/nwprod/lib -lip_4 -lw3nco_4 -lw3emc_4 -lbacio_4 -lsp_4

sref_ens_gen: $(OBJ)
	$(FC)  -o $(@) sref_ens_gen.f $(OBJ) $(LIBS)
#	@cp sref_ens_gen.x ../exe
clean:
	/bin/rm -f sref_ens_gen *.o
clear:
	/bin/rm  *.o
#

