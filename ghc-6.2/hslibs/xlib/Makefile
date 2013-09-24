# -----------------------------------------------------------------------------=
# $Id: Makefile,v 1.7 2003/08/27 15:18:43 panne Exp $
#
# (c) The GHC Team 1997-2002
#

TOP = ..
include $(TOP)/mk/boilerplate.mk

PACKAGE = xlib
PACKAGE_DEPS = greencard

GCDIR		= ../../greencard
GCLIBDIR        = $(GCDIR)/lib/ghc

SRCS            = $(wildcard *.gc)

HS_SRCS		= $(addsuffix .hs, $(basename $(SRCS)))

SRC_HC_OPTS    += -fglasgow-exts -fvia-C -monly-3-regs
SRC_HC_OPTS    += -package greencard
SRC_HC_OPTS    += $(X_CFLAGS)

SRC_GC_OPTS    += --target ghc --include-dir $(GCLIBDIR)

EXTRA_LD_OPTS += $(X_LIBS) -lX11

# yeuch, have to get X_CFLAGS & X_LIBS in through CPP to package.conf.in
comma = ,
PACKAGE_CPP_OPTS += -DX_CFLAGS='$(patsubst %,$(comma)"%",$(X_CFLAGS))'
PACKAGE_CPP_OPTS += -DX_LIBS='$(patsubst %,$(comma)"%",$(X_LIBS))'

#
# Force greencard to be built.
# 
all :: 
	if [ ! -e $(GREENCARD) ]; then \
	   if [ ! -d $(GCDIR) ]; then echo "To build hslibs/xlib, you'll have to checkout fptools/greencard" ; exit 1; fi; \
	    make -C $(GCDIR)/src all; \
	    make -C $(GCDIR)/lib HC=../$(GHC_INPLACE) all; \
	fi;

include $(TOP)/mk/target.mk
