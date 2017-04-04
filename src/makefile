SHELL = /bin/sh
#---------------------------------------------------------------------
# Makefile for anmod
#---------------------------------------------------------------------
# areo (14.10.12):
# It's a hack of a really old makefile for PhD libraries,
# so I didnt make all dependencies and their rules. In principle
# these are the rules that need be invoked if you edit the module
# files, and then need recompile other files that depend on them.
#
# Use:
#	make all: compile
#	make run: run code
#	make plot: plot trace
#	make clean all run plot: Do it all
#---------------------------------------------------------------------
#
F90	= ifort
F77	= ifort
#
ANMOD_ROOT = /private/areo/Src/modeling/analytical_modeling_2d
# 
OBJECTS = bessl.o dataio.o
#
MODULES	= types.o ao.o
F90OPTS = 
#
all:	anmod2d

anmod2d:	$(MODULES) $(OBJECTS) anmod2d.o 
	$(F90) $(F90OPTS) anmod2d.o \
	$(MODULES) $(OBJECTS) -o anmod2d

# The object files depend on modules, ie they must be compiled first
$(OBJECTS):	$(MODULES)

$(MODULES):	

# The main program depend on these, so they must be compiled first
anmod2d.o:	$(MODULES) $(LIBOBJ)	

# Run the code by "make run"
run:
	anmod2d

# plot the traces by "make plot"
plot:
	a2b n1=1 < file.asc | suaddhead ns=1024 | sushw key=dt a=1000 | suxwigb d1=1

# Just a debug plot I used
plottrace:
	a2b n1=1 < trace.a | suaddhead ns=1024 | suxgraph style=normal label1="Sample number" width=600 height=200  d1=1

# clean stuff
clean:
	rm -rf *.o *.mod *.a
	rm -rf anmod2d

#
.f90.o:	; $(F90) -c $(F90OPTS) $<
.f.o:	; $(F90) -c $(F90OPTS) $<
.SUFFIXES:
.SUFFIXES: .o .f90 .f

