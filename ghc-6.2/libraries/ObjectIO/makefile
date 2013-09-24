# -----------------------------------------------------------------------------=
#
# (c) The GHC Team 1997-2000
#

TOP = ..
include $(TOP)/mk/boilerplate.mk

PACKAGE = objectio
PACKAGE_DEPS = base

ALL_DIRS = 	Graphics/UI \
		Graphics/UI/ObjectIO \
		Graphics/UI/ObjectIO/Device \
		Graphics/UI/ObjectIO/Control \
		Graphics/UI/ObjectIO/Menu \
		Graphics/UI/ObjectIO/Process \
		Graphics/UI/ObjectIO/Receiver \
		Graphics/UI/ObjectIO/Timer \
		Graphics/UI/ObjectIO/Window \
		Graphics/UI/ObjectIO/OS

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
PLAT_DIR=Windows
else
PLAT_DIR=Linux
CC_OPTS = -fnative-struct
endif

SRC_HC_OPTS += -fglasgow-exts -Iinclude
SRC_CC_OPTS += -Iinclude -I$(GHC_INCLUDE_DIR)
C_OBJS += $(patsubst %.c, %.o, $(wildcard  cbits/$(PLAT_DIR)/*.c))


STUBOBJS = $(C_OBJS)

#-----------------------------------------------------------------------------
# 	Building the library for GHCi
#
# The procedure differs from that in fptools/mk/target.mk in one way:
#  (*) on Win32 we must split it into four, because a single .o file can't
#      have more than 65536 relocations in it.

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"

# Turn off standard rule which creates HSobjectio.o from LIBOBJS.
DONT_WANT_STD_GHCI_LIB_RULE=YES

GHCI_LIBOBJS = $(HS_OBJS) $(C_OBJS)

INSTALL_LIBS += HSobjectio1.o HSobjectio2.o HSobjectio3.o HSobjectio4.o

endif # TARGETPLATFORM = i386-unknown-mingw32

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk

HSobjectio.o : $(C_OBJS) $(GHCI_LIBOBJS)
	ld -r -x -o HSobjectio1.o $(filter Graphics/UI/ObjectIO/Std%.o,$^)
	ld -r -x -o HSobjectio2.o $(filter-out cbits/$(PLAT_DIR)/%.o, $(filter-out Graphics/UI/ObjectIO.o, $(filter-out Graphics/UI/ObjectIO/Std%.o,$(filter-out Graphics/UI/ObjectIO/OS/%.o,$^))))
	ld -r -x -o HSobjectio3.o Graphics/UI/ObjectIO.o # $(filter-out Graphics/UI/ObjectIO/Std%.o,$(filter-out Graphics/UI/ObjectIO/%Device.o,$(filter Graphics/UI/ObjectIO/%.o,$^)))
	ld -r -x -o HSobjectio4.o $(filter cbits/$(PLAT_DIR)/%.o,$^) $(filter Graphics/UI/ObjectIO/OS/%.o,$^)
	touch HSobjectio.o
