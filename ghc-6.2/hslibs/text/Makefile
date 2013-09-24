# $Id: Makefile,v 1.21.10.1 2003/09/17 09:51:23 simonmar Exp $

TOP = ..
include $(TOP)/mk/boilerplate.mk

PACKAGE = text
PACKAGE_DEPS = lang data

ALL_DIRS = html parsec

SRC_HC_OPTS += -cpp -fvia-C

parsec/ParsecToken_HC_OPTS += -fglasgow-exts
parsec/ParsecPerm_HC_OPTS += -fglasgow-exts

include $(TOP)/mk/target.mk
