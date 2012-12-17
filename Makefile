#MACHINE = dec
#F77 = f77
#FFLGS = -convert big_endian

MACHINE = linux
F77 = pgf77
F90 = pgf90
FFLGS = -byteswapio -Bstatic
OBJ = kcellmod.o

elmask: $(OBJ)
	$(F90) $(FFLGS) -o kcellmod.$(MACHINE) $(OBJ)
.f.o:
	$(F90) -c -o $@ $(FFLGS) $<
clean:
	rm *.o kcellmod.$(MACHINE)
