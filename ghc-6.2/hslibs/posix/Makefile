# $Id: Makefile,v 1.32 2002/12/19 13:52:54 simonmar Exp $

TOP=..
include $(TOP)/mk/boilerplate.mk

PACKAGE = posix
PACKAGE_DEPS = lang unix

SUBDIRS = cbits

SRC_HC_OPTS += -cpp -fglasgow-exts -fvia-C -Icbits

# hsc2hs doesn't care about config.h
ifeq "$(HaveRtldNext)" "YES"
HSC2HS_OPTS += -C-DHAVE_RTLDNEXT
endif
ifeq "$(HaveRtldLocal)" "YES"
HSC2HS_OPTS += -C-DHAVE_RTLDLOCAL
endif
ifeq "$(HaveRtldGlobal)" "YES"
HSC2HS_OPTS += -C-DHAVE_RTLDGLOBAL
endif
ifeq "$(HaveRtldbNow)" "YES"
HSC2HS_OPTS += -C-DHAVE_RTLDNOW
endif

SRC_CC_OPTS += -I../../ghc/includes

include $(TOP)/mk/target.mk
