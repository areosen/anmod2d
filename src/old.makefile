#---------------------------------------------------------------------
# Makefile for anmod.f90
#---------------------------------------------------------------------
.f90.o:	; $(F90) -c $(F90OPTS) $<
.SUFFIXES: .o .f
.SUFFIXES: .o .f90
#
ANMOD_ROOT = /private/k53954/Src/model/anmod
SRC 	= $(ANMOD_ROOT)/src
BIN 	= $(ANMOD_ROOT)/bin
LIB	= $(ANMOD_ROOT)/lib
LIBOBJ  = four90.o bessl.o dataio.o fft.o ffp.o\
	fht.o gaussource.o damping.o
F90OBJ	= types.o ao.o
F90OPTS = -64
F90	= f90
F77	= f77
#
all:	anmod2d anmod3d

anmod2d:	$(SRC)/anmod2d.o $(F90OBJ) $(LIB)/libanmod.a
	$(F90) $(F90OPTS) $(F90OBJ) $(SRC)/anmod2d.o \
	$(LIB)/libanmod.a -o $(BIN)/anmod2d

anmod3d:	$(SRC)/anmod3d.o $(F90OBJ) $(LIB)/libanmod.a
	$(F90) $(F90OPTS) $(F90OBJ) $(SRC)/anmod3d.o \
	$(LIB)/libanmod.a -o $(BIN)/anmod3d

libanmod:	$(LIB)/libanmod.a

$(LIB)/libanmod.a:	 $(LIBOBJ)
	ar cr $(LIB)/libanmod.a $(LIBOBJ)

types.o:	$(SRC)/types.f90 
	$(F90) $(F90OPTS) -c $(SRC)/types.f90

ao.o:	$(SRC)/ao.f90 
	$(F90) $(F90OPTS) -c $(SRC)/ao.f90

anmod2d.o:	$(SRC)/anmod2d.f90 
	$(F90) $(F90OPTS) -c $(SRC)/anmod2d.f90

anmod3d.o:	$(SRC)/anmod3d.f90 
	$(F90) $(F90OPTS) -c $(SRC)/anmod3d.f90

four90.o:	$(SRC)/four90.f90 $(F90OBJ) 
	$(F90) $(F90OPTS) $(F90OBJ) -c $(SRC)/four90.f90

dataio.o:	$(SRC)/dataio.f90 $(F90OBJ) 
	$(F90) $(F90OPTS) $(F90OBJ) -c $(SRC)/dataio.f90

bessl.o:	$(SRC)/bessl.f
	$(F77) -O -64 -c $(SRC)/bessl.f

fft.o:	$(SRC)/fft.f90 four90.o
	$(F90) $(F90OPTS) $(F90OBJ) four90.o -c $(SRC)/fft.f90

ffp.o:	$(SRC)/ffp.f90 four90.o
	$(F90) $(F90OPTS) $(F90OBJ) four90.o -c $(SRC)/ffp.f90

fht.o:	$(SRC)/fht.f90 four90.o
	$(F90) $(F90OPTS) $(F90OBJ) four90.o -c $(SRC)/fht.f90

gaussource.o:	$(SRC)/gaussource.f90 
	$(F90) $(F90OPTS) $(F90OBJ) -c $(SRC)/gaussource.f90

damping.o:	$(SRC)/damping.f90 
	$(F90) $(F90OPTS) $(F90OBJ) -c $(SRC)/damping.f90

clean:
	rm -rf *.o
	rm -rf $(LIB)/*.a
	rm -rf $(BIN)/anmod2d
	rm -rf $(BIN)/anmod3d


