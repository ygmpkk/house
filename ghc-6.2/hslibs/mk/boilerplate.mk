# ##############################################################################
# $Id: boilerplate.mk,v 1.15 2002/02/12 15:17:31 simonmar Exp $
#
# Hslibs boilerplate.mk
# (c) The GHC Team 2002

# Begin by slurping in the boilerplate from one level up.
# Remember, TOP is the top level of the innermost level
# (FPTOOLS_TOP is the fptools top)

# We need to set TOP to be the TOP that the next level up expects!
# The TOP variable is reset after the inclusion of the fptools
# boilerplate, so we stash TOP away first:
HSLIBS_TOP := $(TOP)
TOP:=$(TOP)/..

# Also set GHC_TOP here, because we need to get at bits of GHC's config
GHC_TOP := $(TOP)/ghc

# Pull in GHC's version & project info
-include $(GHC_TOP)/mk/version.mk

# Pull in the fptools boilerplate
include $(TOP)/mk/boilerplate.mk

# Reset TOP
TOP:=$(HSLIBS_TOP)

# -----------------------------------------------------------------
# Everything after this point
# augments or overrides previously set variables.

-include $(GHC_TOP)/mk/paths.mk

-include $(TOP)/mk/paths.mk
-include $(TOP)/mk/opts.mk
-include $(TOP)/mk/suffix.mk
