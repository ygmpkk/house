# -----------------------------------------------------------------------------=
# $Id: Makefile,v 1.4 2003/08/29 16:16:02 panne Exp $
#
# (c) The GHC Team 1997-2000
#

TOP = ..
include $(TOP)/mk/boilerplate.mk

PACKAGE = win32
PACKAGE_DEPS = lang

SplitObjs = NO

GCDIR		= ../../greencard
GCLIBDIR        = $(GCDIR)/lib/ghc

# The GC files that needs to be washed through CPP first.
PGC_SRCS         = $(wildcard *.pgc) 

# The .gc files (but not those that came into being as a result of preprocessing a .pgc file).
GC_SRCS         = $(filter-out $(subst .pgc,.gc,$(PGC_SRCS)), $(wildcard *.gc))

SRCS		 = $(GC_SRCS) $(PGC_SRCS)

# Add the results of pre-processing the .pgc files.
CLEAN_FILES   += $(subst .pgc,.gc,$(PGC_SRCS))

HS_SRCS		 = $(addsuffix .hs, $(basename $(SRCS)))
C_SRCS		 += $(patsubst %.gc,,$(wildcard *.gc))
C_SRCS		 += $(patsubst %.pgc,,$(wildcard *.pgc))
GEN_GCFILES      = Win32Window.gc

STUBOBJS	 += $(patsubst %.c, %.o, $(C_SRCS))
OBJS		 = $(addsuffix .$(way_)o,  $(basename $(SRCS))) errors.o dumpBMP.o WndProc.o diatemp.o finalizers.o
HI_FILES         = $(addsuffix .$(way_)hi,  $(basename $(SRCS)))
LIBOBJS		 += $(OBJS) $(STUB_OBJS)Win32Dialogue_stub.$(way_)o Win32Window_stub.$(way_)o

SRC_CC_OPTS      += -DTARGET_GHC
SRC_HC_OPTS      += -recomp -cpp -fglasgow-exts -fvia-C -optc-DTARGET_GHC
SRC_HC_OPTS      += -fglasgow-exts -package greencard
SRC_GC_OPTS  	 += --target ffi --include-dir $(GCLIBDIR)
SRC_BLD_DLL_OPTS += -luser32 -lgdi32 -ladvapi32 Win32Dialogue_stub.o Win32Window_stub.o

Win32Registry_HC_OPTS += -Onot

ifeq "$(DEBUG)" "YES"
SRC_GC_OPTS      += --stub-debug
SRC_CC_OPTS      += -DDEBUG -O
SRC_HC_OPTS      += -optc-DWIN32_LIB_DEBUG
endif

# Dependency generation
depend :: $(GEN_GCFILES)

depend :: Win32Window.hs

depend :: $(HS_SRCS) $(SRCS) 
	perl mkGCDep *.gc > .depend
	$(HC) -M -optdep-f -optdep.depend $(HC_OPTS) $(MKDEPENDHS_OPTS) $(HS_SRCS)
	$(SED) -e 's#$(GCDIR)/lib/$(GC_TARGET)#$$(GCDIR)/lib/$$(GC_TARGET)#g' < .depend > .depend.fixed && $(RM) .depend && $(MV) .depend.fixed .depend

# Hard-wired dependencies
errors.o  : errors.c
dumpBMP.o : dumpBMP.c
WndProc.o : WndProc.c WndProc.h


include $(TOP)/mk/target.mk
