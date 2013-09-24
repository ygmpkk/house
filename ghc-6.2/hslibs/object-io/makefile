# -----------------------------------------------------------------------------=
#
# (c) The GHC Team 1997-2000
#

TOP = ..
include $(TOP)/mk/boilerplate.mk

PACKAGE = objectio
PACKAGE_DEPS = base

ALL_DIRS = ObjectIO OSWindows OSWindows/Windows_C_12
SRC_HC_OPTS      += -fglasgow-exts -optc-I -optc OSWindows/Windows_C_12
STUBOBJS += $(wildcard  OSWindows/Windows_C_12/*.o)

#-----------------------------------------------------------------------------
# 	Building the library for GHCi
#
# The procedure differs from that in fptools/mk/target.mk in one way:
#  (*) on Win32 we must split it into four, because a single .o file can't
#      have more than 65536 relocations in it.

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"

# Turn off standard rule which creates HSobjectio.o from LIBOBJS.
DONT_WANT_STD_GHCI_LIB_RULE=YES

GHCI_LIBOBJS = $(HS_OBJS) $(STUBOBJS)

INSTALL_LIBS += HSobjectio1.o HSobjectio2.o HSobjectio3.o HSobjectio4.o

endif # TARGETPLATFORM = i386-unknown-mingw32

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk

HSobjectio.o : $(GHCI_LIBOBJS)
	ld -r -x -o HSobjectio1.o $(filter ObjectIO/Std%.o,$^)
	ld -r -x -o HSobjectio2.o $(filter ObjectIO/%Device.o,$^)
	ld -r -x -o HSobjectio3.o $(filter-out ObjectIO/Std%.o,$(filter-out ObjectIO/%Device.o,$(filter ObjectIO/%.o,$^)))
	ld -r -x -o HSobjectio4.o $(filter OSWindows/%.o,$^)
	touch HSobjectio.o

