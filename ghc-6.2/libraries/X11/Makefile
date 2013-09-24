# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.14 2003/06/26 12:36:36 reid Exp $

TOP = .
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------

SUBDIRS = fptools cbits doc

ALL_DIRS = \
	Graphics/X11 \
	Graphics/X11/Xlib

PACKAGE = X11
PACKAGE_DEPS = greencard haskell98

SRC_CC_OPTS += -Iinclude -I.

SRC_HC_OPTS += -cpp -fglasgow-exts -fffi
SRC_HC_OPTS += -Iinclude

SRC_HADDOCK_OPTS += -t "X11 Libraries (${PACKAGE} package)"

# yeuch, have to get X_CFLAGS & X_LIBS in through CPP to package.conf.in
comma = ,
PACKAGE_CPP_OPTS += -DX_CFLAGS='$(patsubst %,"%"$(comma), $(X_CFLAGS))'
PACKAGE_CPP_OPTS += -DX_LIBS='$(patsubst %,  "%"$(comma), $(X_LIBS))'

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
