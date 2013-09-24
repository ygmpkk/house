# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.8 2003/06/06 12:32:34 reid Exp $

TOP = .
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------

# We do not make examples a subdir because then we can't avoid building it
# SUBDIRS = examples
SUBDIRS = 

ALL_DIRS = \
	Graphics \
	Graphics/HGL

ifneq "$(strip $(X_LIBS))" ""

PACKAGE_DEPS   = base X11
SRC_HC_OPTS   += -DPLATFORM=X11
ALL_DIRS      += Graphics/HGL/X11
EXCLUDED_SRCS += Graphics/HGL/Win32.hs

endif

# sleazy test for windows boxes
ifneq "$(strip $(exeext))" ""

PACKAGE_DEPS   = base Win32
SRC_HC_OPTS   += -DPLATFORM=Win32
ALL_DIRS      += Graphics/HGL/Win32
EXCLUDED_SRCS += Graphics/HGL/X11.hs

endif

PACKAGE = HGL

SRC_HC_OPTS += -cpp -fglasgow-exts

SRC_HADDOCK_OPTS += -t "Graphics Libraries (HGL package)"


# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk




