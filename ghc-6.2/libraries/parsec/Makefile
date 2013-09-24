TOP=..
include $(TOP)/mk/boilerplate.mk

SUBDIRS = 

ALL_DIRS = \
	Text/ParserCombinators \
	Text/ParserCombinators/Parsec

PACKAGE = parsec
PACKAGE_DEPS = base

SRC_HC_OPTS += -fglasgow-exts

SRC_HADDOCK_OPTS += -t "Haskell Hierarchical Libraries ($(PACKAGE) package)"

include $(TOP)/mk/target.mk
