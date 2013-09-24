# $Id: Makefile,v 1.10 2002/02/12 15:17:27 simonmar Exp $

TOP = ..
include $(TOP)/mk/boilerplate.mk

PACKAGE = data
PACKAGE_DEPS = lang util

ALL_DIRS = edison edison/Assoc edison/Coll edison/Seq

SRC_HC_OPTS += -cpp -fglasgow-exts
SRC_HC_OPTS += -fallow-undecidable-instances -funbox-strict-fields

include $(TOP)/mk/target.mk
