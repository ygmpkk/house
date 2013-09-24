TOP=..
include $(TOP)/mk/boilerplate.mk

SUBDIRS      = include

ALL_DIRS     = System/Console
PACKAGE      = readline
PACKAGE_DEPS = base

SRC_HC_OPTS += -Iinclude
SRC_CC_OPTS += -Iinclude
SRC_HSC2HS_OPTS += -Iinclude

SRC_HADDOCK_OPTS += -t "Haskell Hierarchical Libraries ($(PACKAGE) package)"

# yeuch, have to get LibsReadline in through CPP to package.conf.in
empty =
space = $(empty) $(empty)
comma = ,
PACKAGE_CPP_OPTS += -DLibsReadline='$(subst $(space),$(comma),$(patsubst %,"%",$(strip $(LibsReadline))))'

# Remove Readline.hs if it is not wanted or headers are not available.
ifeq "$(GhcLibsWithReadline)" "YES"
  ifneq "$(ReadlineIncludePath)" ""
    SRC_HC_OPTS += -I$(ReadlineIncludePath)
    SRC_CC_OPTS += -I$(ReadlineIncludePath)
    SRC_HSC2HS_OPTS += -I$(ReadlineIncludePath)
  endif
ifneq "$(BootingFromHc)" "YES"
  STUBOBJS += System/Console/Readline_stub.$(way_)o
endif
  CLEAN_FILES += System/Console/Readline_stub.[ch] $(STUBOBJS)
else
  EXCLUDED_SRCS += System/Console/Readline.hsc System/Console/Readline_stub.c
endif

include $(TOP)/mk/target.mk
