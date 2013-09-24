# -----------------------------------------------------------------------------=
#
# (c) The GHC Team 2003
#

TOP = ..
include $(TOP)/mk/boilerplate.mk

PACKAGE = japi
PACKAGE_DEPS = base

ALL_DIRS = Graphics/UI/Japi

ifneq "$(NoExamples)" "YES"
SUBDIRS = examples
endif

SRC_HC_OPTS += -fglasgow-exts

include $(TOP)/mk/target.mk
