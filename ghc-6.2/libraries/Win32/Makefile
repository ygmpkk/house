# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.8 2003/06/05 16:13:33 reid Exp $

TOP = .
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------

# Comment out if you want to do initial debugging on Unix systems
SUBDIRS = cbits

ALL_DIRS = \
	System \
	System/Win32 

PACKAGE = Win32
PACKAGE_DEPS = greencard

SRC_CC_OPTS += -Icbits
# SRC_HC_OPTS += -Wall 
SRC_HC_OPTS += -optc-Icbits
SRC_HC_OPTS += -cpp -fglasgow-exts -fffi 

SRC_HADDOCK_OPTS += -t "Win32 Libraries ($(PACKAGE) package)"

# _stub.o files are a side-effect from compiling .hs files that
# contain 'foreign export' declarations.
EXTRA_C_OBJS += System/Win32/Dialogue_stub.o System/Win32/Window_stub.o

STUBOBJS    += $(filter-out $(EXTRA_C_OBJS), $(patsubst %.c, %.o, $(C_SRCS))) $(EXTRA_C_OBJS)

System/Win32/Dialogue_stub.o : System/Win32/Dialogue.o
	@:
System/Win32/Window_stub.o : System/Win32/Window.o
	@:

# -----------------------------------------------------------------------------

DONT_WANT_STD_GHCI_LIB_RULE=YES
include $(TOP)/mk/target.mk

#
# Split into two, as GNU ld isn't capable of handling
# an object file containing this many relocations (>65535).
# sof 2/02.
#
HSWin32.o : $(LIBOBJS) $(STUBOBJS)
	ld -r -x -o HSWin32_1.o $(filter     System/Win32/M% System/Win32/N% System/Win32/R% System/Win32/W%, $^)
	ld -r -x -o HSWin32_2.o $(filter-out System/Win32/M% System/Win32/N% System/Win32/R% System/Win32/W%, $^)
	touch HSWin32.o

ifeq "$(way)" ""
INSTALL_LIBS += HSWin32_1.o HSWin32_2.o
CLEAN_FILES  += HSWin32_1.o HSWin32_2.o
endif


