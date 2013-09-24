# -----------------------------------------------------------------------------
# $Id: bootstrap.mk,v 1.27 2003/09/02 10:23:30 simonmar Exp $
#
# Makefile rules for booting from .hc files without a driver.
#
# When booting from .hc files without a compiler installed, we don't have
# the benefit of the GHC driver to add all the magic options required to
# compile the .hc files, so we have to duplicate that functionality here.
# The result is unfortunately ugly, but we don't have another choice.

TOP_SAVED := $(TOP)
TOP:=$(TOP)/ghc

include $(TOP)/mk/version.mk
include $(TOP)/mk/paths.mk

# Reset TOP
TOP:=$(TOP_SAVED)

# -----------------------------------------------------------------------------
# Set the platform-specific options to send to the C compiler.  These should
# match the list in machdepCCOpts in ghc/compiler/DriverFlags.hs.
#

PLATFORM_CC_OPTS =
PLATFORM_HC_BOOT_CC_OPTS =

ifeq "$(i386_TARGET_ARCH)" "1"
PLATFORM_CC_OPTS += -DDONT_WANT_WIN32_DLL_SUPPORT
PLATFORM_HC_BOOT_CC_OPTS += -fno-defer-pop -fomit-frame-pointer 
endif

ifeq "$(hppa_TARGET_ARCH)" "1"
PLATFORM_CC_OPTS += -static -D_HPUX_SOURCE
endif

ifeq "$(powerpc_TARGET_ARCH)" "1"
PLATFORM_CC_OPTS += -static
PLATFORM_HC_BOOT_CC_OPTS += -finhibit-size-directive
endif

ifeq "$(rs6000_TARGET_ARCH)" "1"
PLATFORM_CC_OPTS += -static
PLATFORM_HC_BOOT_CC_OPTS += -finhibit-size-directive
endif

ifeq "$(mingw32_TARGET_OS)" "1"
PLATFORM_CC_OPTS += -mno-cygwin
endif

ifeq "$(alpha_TARGET_ARCH)" "1"
PLATFORM_CC_OPTS += -static -w
PLATFORM_HC_BOOT_CC_OPTS += -mieee
endif

ifeq "$(sparc_TARGET_ARCH)" "1"
PLATFORM_HC_BOOT_CC_OPTS += -w
endif

ifeq "$(BootingFromUnregisterisedHc)" "YES"
PLATFORM_HC_BOOT_CC_OPTS += -DNO_REGS -DUSE_MINIINTERPRETER
SRC_CC_OPTS += -DNO_REGS -DUSE_MINIINTERPRETER
# Add these flags to SRC_CC_OPTS too, because they need to be passed to the plain .c
# files in ghc/rts.
endif

PLATFORM_CC_OPTS += -D__GLASGOW_HASKELL__=$(ProjectVersionInt) 

HC_BOOT_CC_OPTS = $(PLATFORM_HC_BOOT_CC_OPTS) $(PLATFORM_CC_OPTS) $(CC_OPTS)

SRC_CC_OPTS += -I$(FPTOOLS_TOP_ABS)/ghc/includes -I$(FPTOOLS_TOP_ABS)/libraries/base/include -I$(FPTOOLS_TOP_ABS)/libraries/unix/include

ifeq "$(GhcWithInterpreter)" "YES"
SRC_CC_OPTS += -I$(FPTOOLS_TOP_ABS)/libraries/readline/include
endif

# -----------------------------------------------------------------------------
# Linking: we have to give all the libraries explicitly.

ifeq "$(LeadingUnderscore)" "YES"
UNDERSCORE=_
else
UNDERSCORE=
endif

ifeq "$(HaveLibGmp)" "NO"
DASH_L_GHC_RTS_GMP_DIR=-L$(FPTOOLS_TOP_ABS)/ghc/rts/gmp
endif

HC_BOOT_LD_OPTS =				\
   -L$(FPTOOLS_TOP_ABS)/ghc/rts			\
   $(DASH_L_GHC_RTS_GMP_DIR)                    \
   -L$(FPTOOLS_TOP_ABS)/libraries/base		\
   -L$(FPTOOLS_TOP_ABS)/libraries/base/cbits	\
   -L$(FPTOOLS_TOP_ABS)/libraries/haskell98

ifeq "$(GhcWithInterpreter)" "YES"
HC_BOOT_LD_OPTS += \
   -L$(FPTOOLS_TOP_ABS)/libraries/haskell-src	\
   -L$(FPTOOLS_TOP_ABS)/libraries/readline	\
   -L$(FPTOOLS_TOP_ABS)/libraries/unix          \
   -L$(FPTOOLS_TOP_ABS)/libraries/unix/cbits
endif

HC_BOOT_LD_OPTS += \
   -u "$(UNDERSCORE)GHCziBase_Izh_static_info" \
   -u "$(UNDERSCORE)GHCziBase_Czh_static_info" \
   -u "$(UNDERSCORE)GHCziFloat_Fzh_static_info" \
   -u "$(UNDERSCORE)GHCziFloat_Dzh_static_info" \
   -u "$(UNDERSCORE)GHCziPtr_Ptr_static_info" \
   -u "$(UNDERSCORE)GHCziWord_Wzh_static_info" \
   -u "$(UNDERSCORE)GHCziInt_I8zh_static_info" \
   -u "$(UNDERSCORE)GHCziInt_I16zh_static_info" \
   -u "$(UNDERSCORE)GHCziInt_I32zh_static_info" \
   -u "$(UNDERSCORE)GHCziInt_I64zh_static_info" \
   -u "$(UNDERSCORE)GHCziWord_W8zh_static_info" \
   -u "$(UNDERSCORE)GHCziWord_W16zh_static_info" \
   -u "$(UNDERSCORE)GHCziWord_W32zh_static_info" \
   -u "$(UNDERSCORE)GHCziWord_W64zh_static_info" \
   -u "$(UNDERSCORE)GHCziStable_StablePtr_static_info" \
   -u "$(UNDERSCORE)GHCziBase_Izh_con_info" \
   -u "$(UNDERSCORE)GHCziBase_Czh_con_info" \
   -u "$(UNDERSCORE)GHCziFloat_Fzh_con_info" \
   -u "$(UNDERSCORE)GHCziFloat_Dzh_con_info" \
   -u "$(UNDERSCORE)GHCziPtr_Ptr_con_info" \
   -u "$(UNDERSCORE)GHCziStable_StablePtr_con_info" \
   -u "$(UNDERSCORE)GHCziBase_False_closure" \
   -u "$(UNDERSCORE)GHCziBase_True_closure" \
   -u "$(UNDERSCORE)GHCziPack_unpackCString_closure" \
   -u "$(UNDERSCORE)GHCziIOBase_stackOverflow_closure" \
   -u "$(UNDERSCORE)GHCziIOBase_heapOverflow_closure" \
   -u "$(UNDERSCORE)GHCziIOBase_NonTermination_closure" \
   -u "$(UNDERSCORE)GHCziIOBase_BlockedOnDeadMVar_closure" \
   -u "$(UNDERSCORE)GHCziIOBase_Deadlock_closure" \
   -u "$(UNDERSCORE)GHCziWeak_runFinalizzerBatch_closure" \
   -u "$(UNDERSCORE)__stginit_Prelude"


HC_BOOT_LIBS =

ifeq "$(GhcWithInterpreter)" "YES"
HC_BOOT_LIBS += -lHSreadline -lHShaskell-src -lHSunix -lHSunix_cbits
endif

HC_BOOT_LIBS += -lHShaskell98 -lHSbase -lHSbase_cbits -lHSrts -lgmp -lm $(EXTRA_HC_BOOT_LIBS)

ifeq "$(GhcLibsWithReadline)" "YES"
HC_BOOT_LIBS += $(patsubst %, -l%, $(LibsReadline))
endif

ifeq "$(HaveLibDL)" "YES"
HC_BOOT_LIBS += -ldl
endif
