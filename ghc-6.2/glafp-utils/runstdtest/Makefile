# $Id: Makefile,v 1.5 2000/09/05 10:16:41 simonmar Exp $
#

TOP=..
include $(TOP)/mk/boilerplate.mk

SCRIPT_PROG=runstdtest
SCRIPT_OBJS=runstdtest.prl
SCRIPT_SUBST_VARS=RM DEFAULT_TMPDIR CONTEXT_DIFF
INTERP=perl

CLEAN_FILES += $(SCRIPT_PROG)
DESTDIR=$(INSTSCRIPTDIR)

include $(TOP)/mk/target.mk
