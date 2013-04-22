#MACHINE = dec
#F77 = f77
#FFLGS = -convert big_endian

MACHINE = linux
F77 = pgf77
F90 = pgf90
F95 = pgf95
FFLGS = -byteswapio -Bstatic -mp -g
OBJ = kcellmod.o

elmask: $(OBJ)
	$(F95) $(FFLGS) -o kcellmod.$(MACHINE) $(OBJ)
.f.o:
	$(F95) -c -o $@ $(FFLGS) $<
clean:
	rm *.o kcellmod.$(MACHINE)
