# Makefile for the compilation of the EBT programs ReserveHabSwitch-qc and ReserveHabSwitch-qc_bif
#==============================================================================

# Default configuration flags

EBTDIR      = $(EBTPATH)/integrator
INCLUDEDIRS = . $(EBTDIR) $(EBTDIR)/Odesolvers $(EXTRAINCLUDEDIRS)
TMPDIR      = /tmp

# Generic compilation and linking flags
CC	 = gcc
CFLAGS	 = -O3 -march=native -Wall -Wpointer-arith -Wcast-qual -Wcast-align $(SPECDEFS) -DPROBLEMFILE="<ReserveHabSwitch-qc.h>" $(patsubst %, -I%, $(INCLUDEDIRS))
# CFLAGS	 = -g -DDEBUG=1 -march=native -Wall -Wpointer-arith -Wcast-qual -Wcast-align $(SPECDEFS) -DPROBLEMFILE="<ReserveHabSwitch-qc.h>" $(patsubst %, -I%, $(INCLUDEDIRS))
LD.c     = ld -r
LIBS     = -lm

# Names of the EBT modules linked into the program-independent library
EBTMODS  =  ebtinit ebtmain ebtcohrt ebttint ebtutils ebtstop
EBTOBJS  =  $(TMPDIR)/ebtinit.o $(TMPDIR)/ebtmain.o  $(TMPDIR)/ebtcohrt.o \
	          $(TMPDIR)/ebttint.o $(TMPDIR)/ebtutils.o $(TMPDIR)/ebtstop.o

#==============================================================================

# The dependencies of the executables

ReserveHabSwitch-qc-age: SPECDEFS = -DBIFURCATION=0 -DPROGRAMNAME=RESERVEHABSWITCH-QC-AGE
ReserveHabSwitch-qc:     SPECDEFS = -DBIFURCATION=0 -DPROGRAMNAME=RESERVEHABSWITCH-QC
ReserveHabSwitch-qc_bif: SPECDEFS = -DBIFURCATION=1 -DPROGRAMNAME=RESERVEHABSWITCH-QC

ReserveHabSwitch-qc-age: ReserveHabSwitch-qc-age.o ReserveHabSwitch-qc-age.lib.o
	$(LINK.c) $(LDFLAGS) -o $@ ReserveHabSwitch-qc-age.o     ReserveHabSwitch-qc-age.lib.o     $(LIBS)

ReserveHabSwitch-qc: ReserveHabSwitch-qc.o ReserveHabSwitch-qc.lib.o
	$(LINK.c) $(LDFLAGS) -o $@ ReserveHabSwitch-qc.o     ReserveHabSwitch-qc.lib.o     $(LIBS)

ReserveHabSwitch-qc_bif: ReserveHabSwitch-qc_bif.o ReserveHabSwitch-qc_bif.lib.o
	$(LINK.c) $(LDFLAGS) -o $@ ReserveHabSwitch-qc_bif.o ReserveHabSwitch-qc_bif.lib.o $(LIBS)

#==============================================================================

# The dependencies of the problem-specific object files

ReserveHabSwitch-qc-age.o: ReserveHabSwitch-qc-age.c ReserveHabSwitch-qc-age.h $(EXTRADEPENDS)
	$(COMPILE.c) -o $@ ReserveHabSwitch-qc-age.c

ReserveHabSwitch-qc.o: ReserveHabSwitch-qc.c ReserveHabSwitch-qc.h $(EXTRADEPENDS)
	$(COMPILE.c) -o $@ ReserveHabSwitch-qc.c

ReserveHabSwitch-qc_bif.o: ReserveHabSwitch-qc.c ReserveHabSwitch-qc.h $(EXTRADEPENDS)
	$(COMPILE.c) -o $@ ReserveHabSwitch-qc.c

#==============================================================================

# The dependencies of the problem object library file

ReserveHabSwitch-qc-age.lib.o: ReserveHabSwitch-qc-age.h
	for I in $(EBTMODS) ; do $(COMPILE.c) -o $(TMPDIR)/$${I}.o $(EBTDIR)/$${I}.c ; done
	$(LD.c) -o $@ $(EBTOBJS)
	@${CLEANCMD}

ReserveHabSwitch-qc.lib.o: ReserveHabSwitch-qc.h
	for I in $(EBTMODS) ; do $(COMPILE.c) -o $(TMPDIR)/$${I}.o $(EBTDIR)/$${I}.c ; done
	$(LD.c) -o $@ $(EBTOBJS)
	@${CLEANCMD}

ReserveHabSwitch-qc_bif.lib.o: ReserveHabSwitch-qc.h
	for I in $(EBTMODS) ; do $(COMPILE.c) -o $(TMPDIR)/$${I}.o $(EBTDIR)/$${I}.c ; done
	$(LD.c) -o $@ $(EBTOBJS)
	@${CLEANCMD}

#==============================================================================

# The dependencies of some additional targets

clean:
	@echo "Cleaning up RReserveHabSwitch-qc-age, ReserveHabSwitch-qc and ReserveHabSwitch-qc_bif"
	@rm -f  ReserveHabSwitch-qc-age  ReserveHabSwitch-qc-age.o    ReserveHabSwitch-qc-age.lib.o ReserveHabSwitch-qc-agemodule.*
	@rm -f  ReserveHabSwitch-qc      ReserveHabSwitch-qc.o        ReserveHabSwitch-qc.lib.o     ReserveHabSwitch-qcmodule.*
	@rm -f  ReserveHabSwitch-qc_bif  ReserveHabSwitch-qc_bif.o    ReserveHabSwitch-qc_bif.lib.o ReserveHabSwitch-qc_bifmodule.*
	@for I in $(EBTMODS) ; do rm -f ./$${I}.o $(TMPDIR)/$${I}.o ; done

#==============================================================================
