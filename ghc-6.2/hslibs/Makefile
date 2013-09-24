# -----------------------------------------------------------------------------=
# $Id: Makefile,v 1.30 2002/09/20 14:16:41 simonmar Exp $
# (c) The GHC Team 1997-2000

TOP = .
include $(TOP)/mk/boilerplate.mk

ifeq "$(BootingFromHc)" "YES"
SUBDIRS = lang concurrent posix util data text 
else
# order is important here, there are dependencies between the libraries...
ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
PACKAGE_POSIX=
PACKAGE_WIN32=win32
else
PACKAGE_POSIX=posix
PACKAGE_WIN32=
endif

SUBDIRS = lang concurrent $(PACKAGE_POSIX) util data text net hssource $(PACKAGE_WIN32) doc

endif

include $(TOP)/mk/target.mk
