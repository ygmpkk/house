# $Id: Makefile,v 1.19 2002/06/20 16:02:03 simonmar Exp $

TOP = ../..
include $(TOP)/mk/boilerplate.mk

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
EXCLUDED_SRCS += selectFrom.c
endif

SRC_CC_OPTS += -Wall
SRC_CC_OPTS += -I$(GHC_INCLUDE_DIR) -I$(GHC_RUNTIME_DIR) \
		-I$(GHC_LIB_DIR)/base/include

LIBRARY = libHSutil_cbits.a
LIBOBJS = $(C_OBJS)

includedir = $(libdir)/include
INSTALL_INCLUDES = HsUtil.h

include $(TOP)/mk/target.mk
