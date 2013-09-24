# -----------------------------------------------------------------------------=
#
# (c) The GHC Team 1997-2000
#

TOP = ..
include $(TOP)/mk/boilerplate.mk

PACKAGE = win32
PACKAGE_DEPS = lang

# _stub.o files are a side-effect from compiling .hs files that
# contain 'foreign export' declarations.
EXTRA_C_OBJS += Win32Dialogue_stub.o Win32Window_stub.o

STUBOBJS    += $(filter-out $(EXTRA_C_OBJS), $(patsubst %.c, %.o, $(C_SRCS))) $(EXTRA_C_OBJS)

# GCDIR		  = ../../greencard
# GCLIBDIR          = $(GCDIR)/lib/ghc

SRC_CC_OPTS      += -DTARGET_GHC -I$(GHC_INCLUDE_DIR)
SRC_HC_OPTS      += -cpp -fvia-C -optc-DTARGET_GHC -fglasgow-exts 

# SRC_GC_OPTS  	 += --target ffi --include-dir $(GCLIBDIR)
# SRC_BLD_DLL_OPTS += -luser32 -lgdi32 -ladvapi32 Win32Dialogue_stub.o Win32Window_stub.o

# ifeq "$(DEBUG)" "YES"
# SRC_GC_OPTS      += --stub-debug
# SRC_CC_OPTS      += -DDEBUG -O
# SRC_HC_OPTS      += -optc-DWIN32_LIB_DEBUG
# endif

Win32Dialogue_stub.o : Win32Dialogue.o
	@:
Win32Window_stub.o : Win32Window.o
	@:

# Hard-wired dependencies
errors.o  : errors.c
dumpBMP.o : dumpBMP.c
WndProc.o : WndProc.c WndProc.h

DONT_WANT_STD_GHCI_LIB_RULE=YES
include $(TOP)/mk/target.mk

#
# Split into two, as GNU ld isn't capable of handling
# an object file containing this many relocations (>65535).
# sof 2/02.
#
HSwin32.o : $(LIBOBJS) $(STUBOBJS)
	ld -r -x -o HSwin321.o $(filter     Win32M% Win32N% Win32R% Win32W%, $^)
	ld -r -x -o HSwin322.o $(filter-out Win32M% Win32N% Win32R% Win32W%, $^)
	touch HSwin32.o

ifeq "$(way)" ""
INSTALL_LIBS += HSwin321.o HSwin322.o
CLEAN_FILES  += HSwin321.o HSwin322.o
endif

