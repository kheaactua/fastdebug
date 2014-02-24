.SUFFIXES : .c .o .f .ftn .ftn90

SHELL = /bin/bash

.PHONY: testf testc lib stubs
.DEFAULT: lib

# Default optimization
OPTIL?=2

# Are we in an Env Can environment?
ENV_CAN=1
ifeq ($(ENV_CAN),1)
FC:=s.f90 -mpi -O$(OPTIL)
CC:=s.cc -mpi -O$(OPTIL)
else
# Not tested yet
FC:=pgfortran -O$(OPTIL)
CC:=pgcc -mpi -O$(OPTIL)
endif

.f90.o:
	$(FC) -c $<
.f.o:
	$(FC) -c $<
.ftn90.o:
	rm -f $*.f90
	$(FC) -c $<
#.ftn90.f90:
#	rm -f $*.f90
#	$(FTNC) $<
.c.o:
	$(CC) -c $<
.s.o:
	$(AS) -c $(CPPFLAGS) $(ASFLAGS) $<


#   Common recipes for models
#
# Matt: added the /etc/ here to match the pre-publish
#include $(MODEL_PATH)/etc/Makefile_$(BASE_ARCH)

#bidon: mattdebug.o
#	makebidon mattmain;\
#	s.compile -obj bidon.o $^ -debug -o test $(OMP) $(MPI);\
#	rm -f bidon.o
#

lib: fastdebug.o
	ar rcs libfastdebug.a fastdebug.o

stubs:
	@echo "Not yet implemented"

#test2:
#	s.f90 -mpi -O0 -g -c test_mattdebug.F90
#	s.cc -mpi -O0 -g -c mattdebug.c
#	s.f90 -mpi -o test -O0 -g mattdebug.o test_mattdebug.o
#	rm test_mattdebug.o

testf:
	$(FC) -c test.F90
	$(CC) -c fastdebug.c
	$(FC) -o test fastdebug.o test.o
	rm test.o

testc:
	mpicc -o test -DTEST=1 fastdebug.c
