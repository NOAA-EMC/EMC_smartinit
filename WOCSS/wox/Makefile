FC=ifort
# FFLAGS    = -f77rtl -O0 -convert big_endian
FFLAGS    = -f77rtl -O0 
CORELIB  = /nwprod/lib
NETCDFPATH = /usrx/local/NetCDF/3.6.3
INCnetcdf       = $(NETCDFPATH)/include
LIBS = -L$(NETCDFPATH)/lib -lnetcdf



wox: wox.o write_netcdf.o
	$(FC) -o wox $(FFLAGS) wox.o write_netcdf.o $(LIBS)

wox.o : wox.f write_netcdf.f
	$(FC) -c $(FFLAGS) wox.f write_netcdf.f -I$(INCnetcdf)
clean:
	rm -f wox.o write_netcdf.o wox


#/usr/bin/g77 -c -O -Wno-globals wox.f write_netcdf.f -I/usr/local/netcdf/include
#/usr/bin/g77 -O -Wno-globals -o wox wox.o write_netcdf.o -L/usr/local/netcdf/lib -lnetcdf 
