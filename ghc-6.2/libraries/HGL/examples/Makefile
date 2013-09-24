# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.3 2003/06/04 12:51:30 reid Exp $

TOP = ..
include $(TOP)/mk/boilerplate.mk

#
# Disable 'make boot'
#
NO_BOOT_TARGET=YES

WAYS=

# -----------------------------------------------------------------------------

EXAMPLES    := $(wildcard *.hs)
BINS        := $(EXAMPLES:.hs=)
CLEAN_FILES += $(addsuffix $(exeext), $(BINS))

SRC_HC_OPTS += -package HGL -package haskell98

all:: $(BINS)

$(BINS): %$(exeext): %.hs
	$(HC) -o $@ $(HC_OPTS) $(LD_OPTS) $<

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
