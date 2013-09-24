# $Id: target.mk,v 1.38 2002/07/02 08:41:47 simonmar Exp $

# One addition to HC_OPTS: these are old-style libraries with flat
# module names, so we need to explicitly search in the subdirectories
# for modules.
SRC_HC_OPTS += $(addprefix -i, $(ALL_DIRS))

#
# Force the use of GHC_INPLACE for tools/
ifeq "$(PACKAGE)" ""
HC 	     	= $(GHC_INPLACE)
endif

# We don't use Haddock in hslibs
NO_HADDOCK_DOCS = YES

TOP:=$(TOP)/..
include $(TOP)/mk/target.mk
TOP:=$(HSLIBS_TOP)
