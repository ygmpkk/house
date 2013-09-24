# $Id: Makefile,v 1.13 2002/06/20 16:02:02 simonmar Exp $

TOP = ../..
include $(TOP)/mk/boilerplate.mk

SRC_CC_OPTS += -Wall -DCOMPILING_STDLIB 
SRC_CC_OPTS += -I$(GHC_INCLUDE_DIR) -I$(GHC_RUNTIME_DIR) \
		-I$(GHC_LIB_DIR)/base/include

LIBRARY = libHSlang_cbits.a
LIBOBJS = $(C_OBJS)

includedir = $(libdir)/include
INSTALL_INCLUDES = HsLang.h

include $(TOP)/mk/target.mk
