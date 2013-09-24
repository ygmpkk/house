#								 -*-makefile-*-
# mk/config.mk.  Generated from config.mk.in by configure.
#
################################################################################
#
# config.mk.in
#
# This file supplies defaults for many tweakable build configuration
# options.  Some of the defaults are filled in by the autoconf-generated
# configure script.
#
# DO NOT EDIT THIS FILE!
#
# 	- config.mk is auto-generated from config.mk.in by configure.
#	  This may be triggered automatically by the build system (say
#	  if config.mk.in has changed more recently than config.mk),
#	  so if you edit config.mk your changes will be spammed.
#
#	- Settings in this file may be overriden by giving replacement
#	  definitions in build.mk.  See build.mk.sample for a good
#	  starting point for a build.mk file.
#
#	  If you don't have a build.mk file then you get defaults for everything.
#	  The defaults should provide a reasonable vanilla build.
#
# This is one of only two files that configure generates (the other is config.h)
#
# There is a section below for each project within the fptools suite
#
#	PLUS
#
# a section corresponding to each of the main .mk files 
# included by boilerplate.mk (see boilerplate.mk for a list).


################################################################################
#
#		Project-wide platform variables
#
################################################################################

# A "platform" is the GNU cpu-type/manufacturer/operating-system target machine
# specifier.  E.g. sparc-sun-solaris2
#
# Build  platform: the platform on which we are doing this build
# Host   platform: the platform on which these binaries will run
# Target platform: the platform for which this compiler will generate code
#
# We don't support build & host being different, because the build
# process creates binaries that are run during the build, and also
# installed.
#
# If host & target are different, then we are building a compiler
# which will generate intermediate .hc files to port to the target
# architecture for bootstrapping.  The libraries and stage 2 compiler
# will be built as HC files for the target system, and likely won't
# build on this host platform.
#
# Guidelines for when to use HOST vs. TARGET:
#
#  - In the build system (Makefile, foo.mk), normally we should test
#    $(HOSTPLATFORM).  There are some cases (eg. installation), where
#    we expect $(HOSTPLATFORM)==$(TARGETPLATFORM), so in those cases it
#    doesn't matter which is used.
#
#  - In the compiler itself, we should test HOST or TARGET depending
#    on whether the conditional relates to the code being generated, or
#    the platform on which the compiler is running.  For stage 2,
#    HOSTPLATFORM should be reset to be TARGETPLATFORM (we currently
#    don't do this, but we should).
#
#  - In the RTS and library code, we should be testing TARGET only.
#
# NOTE: cross-compiling is not well supported by the build system.
# You have to do a lot of work by hand to cross compile: see the
# section on "Porting GHC" in the Building Guide.

HOSTPLATFORM			= i386-unknown-linux
TARGETPLATFORM			= i386-unknown-linux
BUILDPLATFORM			= i386-unknown-linux

# Hack alert:
# in one or two places, we need to get at the OS version (major and
# perhaps even minor), HostOS_Full is the OS name reported by
# AC_CANONICAL_SYSTEM.
#
HostPlatform_CPP		= i386_unknown_linux
HostArch_CPP			= i386
HostOS_CPP			= linux
HostOS_Full			= linux-gnu
HostVendor_CPP	                = unknown

TargetPlatform_CPP		= i386_unknown_linux
TargetArch_CPP			= i386
TargetOS_CPP			= linux
TargetVendor_CPP                = unknown

BuildPlatform_CPP		= i386_unknown_linux
BuildArch_CPP			= i386
BuildOS_CPP			= linux
BuildVendor_CPP                 = unknown

i386_unknown_linux_HOST           = 1
i386_unknown_linux_TARGET       = 1
i386_unknown_linux_BUILD         = 1

i386_HOST_ARCH          = 1
i386_TARGET_ARCH      = 1
i386_BUILD_ARCH        = 1

linux_HOST_OS              = 1
linux_TARGET_OS          = 1
linux_BUILD_OS            = 1

unknown_HOST_VENDOR      = 1
unknown_TARGET_VENDOR  = 1
unknown_BUILD_VENDOR    = 1

# Leading underscores on symbol names in object files
# Valid options: YES/NO
#
LeadingUnderscore=NO

# Pin a suffix on executables? If so, what (Windows only).
exeext=

################################################################################
#
#		project-wide flags
#
# 	Set of options applicable to all fptools projects
#
################################################################################

# BootingFromHc - build GHC and the libraries from .hc files?
BootingFromHc = NO

# BootingFromUnregisterisedHc - treat .hc files as containing unregisterised
# rather than registerised code, i.e., disable the mangler?
BootingFromUnregisterisedHc = NO

# Build Order: we build Happy, Haddock and Alex before GHC if they are
# in this source tree, just in case our GHC build depends on these
# local builds rather than installed versions of the tools.
#
# Build the libs first if we're bootstrapping from .hc files.
ifeq "$(BootingFromHc)" "YES"
AllProjects = glafp-utils happy alex haddock libraries hslibs ghc greencard hdirect hood nofib
else
AllProjects = glafp-utils happy alex haddock ghc libraries hslibs greencard hdirect hood nofib
endif

#
# (OPTIONAL) set ProjectsToBuild to a list of projects to be built.  If this
# list is empty, then all projects present in the source tree will be built.
#
ProjectsToBuild =

#
# set ProjectsDontInstall to a list of projects which are normally built but
# not installed.
#
ProjectsDontInstall = glafp-utils nofib

#
# Should the various project tests directories be built?
#
IncludeTestDirsInBuild=NO

#
# Which ways should SGML documents be built?
# options are: dvi ps pdf html rtf
#
SGMLDocWays=

################################################################################
#
#		GHC project
# 
# 	Set of (configurable) options needed by the ghc tree
#	plus their default options (if any).
#
################################################################################

#---------------------------------------------------------------
#
# Variables that control how the compiler itself is built
#
#---------------------------------------------------------------

# The compiler used to build GHC is $(GHC).  To change the actual compiler
# used, re-configure with --with-ghc=<path-to-ghc>.

# Extra ways in which to build the compiler (for example, you might want to
# build a profiled compiler so you can see where it spends its time)
GhcCompilerWays=

# Extra option flags to pass to the compiler that compiles the compiler
# (Ones that are essential are wired into ghc/compiler/Makefile)
# Typical options to use here:
#
#	-DDEBUG		include debugging code and assertions (will make the
#			compiler slower and produce debugging output, but useful
#			for development)
#
#	-dcore-lint	check the types after every pass of the compiler;
#			a pretty strong internal check of the compiler being
#			used to compile GHC.  Useful when bootstrapping.
GhcHcOpts=-Rghc-timing

# Extra options added to specific stages of the compiler bootstrap.
# These are placed later on the command line, and may therefore
# override options from $(GhcHcOpts).
GhcStage1HcOpts=
GhcStage2HcOpts=
GhcStage3HcOpts=

# Build a compiler that will build *unregisterised* libraries and
# binaries by default.  Unregisterised code is supposed to compile and
# run without any support for architecture-specific assembly mangling,
# register assignment or tail-calls, and is therefore a good way to get
# started when porting GHC to new architectures.
#
# If this is set to NO, you can still use the unregisterised way
# (way 'u') to get unregisterised code, but the default way will be
# registerised.
#
# NOTE: the stage1 compiler will be a registerised binary (assuming
# the compiler you build with is generating registerised binaries), but
# the stage2 compiler will be an unregisterised binary.
#
GhcUnregisterised=NO

# Build a compiler with a native code generator backend
# (as well as a C backend)
#
# Target platforms supported:
#   i386, sparc & powerpc
ifneq "$(findstring $(HostArch_CPP), i386 sparc powerpc)" ""
GhcWithNativeCodeGen=YES
else
GhcWithNativeCodeGen=NO
endif

# Include support for generating Java
GhcWithJavaGen=NO

HaveLibDL = 
HaveRtldNext = 
HaveRtldLocal = 

# Include GHCi in the compiler.  Default to NO for the time being.

ifneq "$(findstring $(HostOS_CPP), mingw32 cygwin32 linux solaris2 freebsd netbsd openbsd darwin)" ""
GhcWithInterpreter=YES
else 
GhcWithInterpreter=NO
endif

#
# Building various ways?
# (right now, empty if not).
BuildingParallel=$(subst mp,YES,$(filter mp,$(WAYS)))
BuildingGranSim=$(subst mg,YES,$(filter mg,$(WAYS)))

#------------------------------------------------------------------------------
# Options for Libraries

# What extra ways to build the libraries in
# In addition to the normal sequential way, the default is to also build
# profiled prelude libraries.
# When booting from .hc files, turn this off.
ifeq "$(BootingFromHc)" "YES"
GhcLibWays=
else
GhcLibWays=p
endif

# Option flags to pass to GHC when it's compiling modules in
# fptools/libraries.  Typically these are things like -O or
# -dcore-lint or -H32m.  The ones that are *essential* are wired into
# the build system.
#
# 	-O is pretty desirable, otherwise no inlining of prelude
#		things (incl "+") happens when compiling with this compiler

GhcLibHcOpts=-O -Rghc-timing

# Win32 only: Enable the RTS and libraries to be built as DLLs
DLLized=@EnableWin32DLLs@

# Win32 only: are we building a compiler that tries to reduce external
# dependencies? i.e., one that doesn't assume that the user has got
# the cygwin toolchain installed on his/her Win32 box.
#
# GHC is still dependent on GNU tools in the backend (gcc to further process
# .c/.hc/.s/.o files + 'perl' to mangle and split), but using this
# option a GHC distribution can be put together which includes a minimal
# set of these open source tools. 
#
MinimalUnixDeps=@MinimalUnixDeps@

# Strip local symbols from libraries?  This can make the libraries smaller,
# but makes debugging somewhat more difficult.  Doesn't work with all ld's.
#
StripLibraries=NO


# ----------------------------------------------------------------------------
# Object-file splitting
#
# 	Set SplitObjs=YES or NO in your build.mk
#
#	Don't use -split-objs in in GhcLibHcOpts, because the build
#		system needs to do other special magic if you are
#		doing object-file splitting

# Don't split object files for libs if we're building DLLs, or booting from
# .hc files.
SplitObjs=YES

ifeq "$(DLLized)" "YES"
SplitObjs=NO
endif
ifeq "$(BootingFromHc)" "YES"
SplitObjs=NO
endif
ifeq "$(GhcUnregisterised)" "YES"
SplitObjs=NO
endif
ifeq "$(TARGETPLATFORM)" "ia64-unknown-linux"
SplitObjs=NO
endif

# ----------------------------------------------------------------------------
# Options for GHC's RTS

# This is a good way to set things like -optc-g and -optc-DDEBUG for the RTS.
# GhcRtsHcOpts is used when compiling .hc files and .c files.
# GhcRtsCcOpts is used when compiling .c  files only.

# For a debugging RTS:
# GhcRtsHcOpts = -optc-DDEBUG
# GhcRtsCcOpts = -g

# For an optimised RTS:
GhcRtsHcOpts=-O2
GhcRtsCcOpts=-fomit-frame-pointer

# Include the front panel code?  Needs GTK+.
GhcRtsWithFrontPanel = NO

#
# To have the RTS support interoperation with OS threads, set
# GhcRtsThreaded to YES (preferably via the --enable-threaded-rts
# configure script option). The consequence of this is spelled out
# in details elsewhere, but, briefly, Concurrent Haskell threads
# can now make external (i.e., C) calls without blocking the progress
# of other CH threads. Multiple native threads can also execute
# Haskell code without getting in each others way too.
# 
GhcRtsThreaded=NO

GhcRtsStandalone=YES

################################################################################
#
# libraries project
#
################################################################################

# Build the Haskell Readline bindings?
#
GhcLibsWithReadline=

# Libraries needed for linking with readline
LibsReadline=

# Include path to readline.h
# (no path == in standard include path)
#
ReadlineIncludePath=

# Math library
LIBM=-lm

# Build the ObjectIO ?
#
GhcLibsWithObjectIO=NO

# Build the Haskell OpenGL binding?
#
GhcLibsWithOpenGL=NO
GL_CFLAGS=
GL_LIBS=

# Build the Haskell GLUT binding?
#
GhcLibsWithGLUT=NO
GLUT_LIBS=

# X11 stuff
#
X_CFLAGS=
X_LIBS=

# .NET interop support?
#
DotnetSupport=NO

# Build unix package?
#
GhcLibsWithUnix=YES

################################################################################
#
#		happy project
#
# 	Happy specific options
#
################################################################################

# The compiler you'd like to use to compile Happy
WithHappyHc = /usr/bin/ghc

# HappyHcOpts gives the flags to pass to the Haskell compiler used
# 	      to compile the Happy sources with.
#
HappyHcOpts = -O

################################################################################
#
#		haggis project
#
# 	Haggis specific options
#
################################################################################

################################################################################
#
#		greencard project
#
# 	GreenCard specific options
#
################################################################################

################################################################################
#
#		nofib project
#
# 	nofib specific options
#
################################################################################

WithNofibHc = $(GHC_INPLACE)

# NoFibSubDirs controls which set of tests should be run
# You can run one or more of
#	imaginary 
#	spectral
#	real
#	parallel
#	PRIVATE
#	PENDING
#	UNUSED
NoFibSubDirs = imaginary spectral real

# The different ways to build nofib. Default is just to mirror
# what is done for the ghc prelude libraries.
#
NoFibWays = $(GhcLibWays)

# Haskell compiler options for nofib
NoFibHcOpts = -O

# ==============================================================================
#
#			END OF PROJECT-SPECIFIC STUFF
#
#		Now come the generic configuration options
#
# ==============================================================================

################################################################################
#
#		Paths (see paths.mk)
#
################################################################################

# Directory used by GHC (and possibly other tools) for storing
# temporary files.  If your TMPDIR isn't big enough, either override
# this in build.mk or set your environment variable "TMPDIR" to point
# to somewhere with more space.  (TMPDIR=. is a good choice).

# DEFAULT_TMPDIR isn't called TMPDIR because GNU make tends to
# override an environment variable with the value of the make variable
# of the same name (if it exists) when executing sub-processes, so
# setting the TMPDIR env var would have no effect in the build tree.

DEFAULT_TMPDIR		= /tmp
ifeq "$(TARGETPLATFORM)" "i386-unknown-cygwin32"
DEFAULT_TMPDIR		= C:/TEMP
endif
ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
DEFAULT_TMPDIR		= C:/TEMP
endif

# FPTOOLS_TOP: the top of the fptools hierarchy, absolute path. (POSIX / unix-style path).
FPTOOLS_TOP_ABS		= /local/home/hallgren/src/House/hOp-2004-06-09/ghc-6.2
# The platform specific version of 'hardtop'.
FPTOOLS_TOP_ABS_PLATFORM = /local/home/hallgren/src/House/hOp-2004-06-09/ghc-6.2

#
# Installation directories, we don't use half of these,
# but since the configure script has them on offer while
# passing through, we might as well set them.

prefix			= /usr/local
exec_prefix		= ${prefix}
bindir			= ${exec_prefix}/bin
datadir0		= ${prefix}/share

#sysconfdir		= ${prefix}/share
#sharedstatedir		= ${prefix}/com
#localstatedir		= ${prefix}/var
libdir0			= ${exec_prefix}/lib
includedir		= ${prefix}/include
oldincludedir		= /usr/include
mandir			= ${prefix}/man

#UNUSED:infodir		= ${prefix}/info
#UNUSED:srcdir          = .

#
# override libdir and datadir to put project-specific stuff in
# a subdirectory with the version number included.
#
libdir     = $(if $(ProjectNameShort),$(libdir0)/$(ProjectNameShort)-$(ProjectVersion),$(libdir0))
datadir    = $(if $(ProjectNameShort),$(datadir0)/$(ProjectNameShort)-$(ProjectVersion),$(datadir0))

# Default place for putting interface files is $(libdir)
# (overriden for packages in package.mk)
ifacedir   = $(libdir)

# 
# Default values for most of the above are only set if
# they weren't configured to anything in particular
# via the configure script. (i.e., we make no assumption
# that the autoconf-generated script will assign defaults
# to all of the above).
#
ifeq "$(strip $(exec_prefix))" ""
exec_prefix		= $(prefix)
endif
ifeq "$(strip $(bindir))" ""
bindir			= $(exec_prefix)/bin
endif

#
# NOTE: by intention, libexecdir and libdir point to 
# the same place.
#  => Only way to override this is to set libexecdir= on the command line.
#     (NOTE: configure script setting is ignored).
libexecdir		= $(libdir)

ifeq "$(strip $(datadir))" ""
datadir		= $(prefix)/share
endif
ifeq "$(strip $(libdir))" ""
libdir		= $(exec_prefix)/lib
endif
ifeq "$(strip $(includedir))" ""
includedir	= $(prefix)/include
endif
ifeq "$(strip $(oldincludedir))" ""
oldincludedir	= /usr/include
endif
ifeq "$(strip $(mandir))" ""
mandir		= $(prefix)/man
endif

################################################################################
#
#		Utilities programs: flags
#
################################################################################

# If you want to give any standard flags to pretty much any utility
# (see utils.mk for a complete list), by adding a line here
#
# 	SRC_P_OPTS += ...
#
# where P is the utility. For example, to add -O to all Haskell
# compilations, 
#
#	SRC_HC_OPTS += -O

SRC_HC_OPTS += -H16m -O

# These flags make flex 8-bit
SRC_FLEX_OPTS	+= -8

SRC_INSTALL_BIN_OPTS	+= -s

# lint gets all CPP's flags too
SRC_LINT_OPTS		+= -axz -DLINT $(SRC_CPP_OPTS)
WAY$(_way)_LINT_OPTS	+= WAY$(_way)_CPP_OPTS

# Default fptools options for dllwrap.
SRC_BLD_DLL_OPTS += -mno-cygwin --target=i386-mingw32

# Flags for CPP when running GreenCard on .pgc files
GC_CPP_OPTS += -P -E -x c -traditional -D__GLASGOW_HASKELL__


################################################################################
#
#		Utilities programs: where to find them
#
################################################################################

#-----------------------------------------------------------------------------
# FPtools Utility locations

# By default, the various utils needed to be build ghc and chums
# is set up to point to the utils/ directory. Define here the
# path prefix for the utilities. Notice that it's a prefix with
# a trailing slash, so that it can be concatenated directly on
# front of a program name; if it's not set then we just look
# along the shell's $(PATH)
#
# If instead you want to use installed or your own versions of these,
# override the various *_PREFIX in build.mk, i.e., having the following
# in build.mk:
#
#   MKDEPENDC_PREFIX=
#
# will force `make' to rummage around in your PATH to find `mkdependC' (not
# sure it would need it in the first place, but still).
#
GLAFP_UTILS  		= $(FPTOOLS_TOP)/glafp-utils

SCRIPT_PREFIX 		= $(GLAFP_UTILS)/scripts/
MKDEPENDC_PREFIX	= $(GLAFP_UTILS)/mkdependC/
LTX_PREFIX		= $(GLAFP_UTILS)/ltx/
RUNTEST_PREFIX		= $(GLAFP_UTILS)/runstdtest/
VERBATIM_PREFIX		= $(GLAFP_UTILS)/verbatim/
SGMLVERB_PREFIX		= $(GLAFP_UTILS)/sgmlverb/
ETAGS_PREFIX		= $(GLAFP_UTILS)/etags/
LNDIR_PREFIX		= $(GLAFP_UTILS)/lndir/
MKDIRHIER_PREFIX	= $(GLAFP_UTILS)/mkdirhier/
DOCBOOK_PREFIX		= $(GLAFP_UTILS)/docbook/

HADDOCK_PREFIX		= $(FPTOOLS_TOP)/haddock/

LITERATE_PREFIX    	= $(FPTOOLS_TOP)/literate/

UNLIT_PREFIX    	= $(FPTOOLS_TOP)/ghc/utils/unlit/
HP2PS_PREFIX            = $(FPTOOLS_TOP)/ghc/utils/hp2ps/
HSTAGS_PREFIX           = $(FPTOOLS_TOP)/ghc/utils/hasktags/
HSC2HS_PREFIX		= $(FPTOOLS_TOP)/ghc/utils/hsc2hs/

#-----------------------------------------------------------------------------
# Haskell compilers and mkdependHS

# $(GHC), $(HBC) and $(NHC) point to installed versions of the relevant
# compilers, if available.
#
# $(HC) is a generic Haskell 98 compiler, set to $(GHC) by default.
# $(MKDEPENDHS) is the Haskell dependency generator (ghc -M).
#
# NOTE: Don't override $(GHC) in build.mk, use configure --with-ghc instead
# (because the version numbers have to be calculated).

GHC		= /usr/bin/ghc
GhcVersion	= 6.2.1
GhcMajVersion	= 6
GhcMinVersion	= 2
GhcPatchLevel	= 1

# Canonicalised ghc version number, used for easy (integer) version
# comparisons.  We must expand $(GhcMinVersion) to two digits by
# adding a leading zero if necessary:
ifneq "$(findstring $(GhcMinVersion), 0 1 2 3 4 5 6 7 8 9)" ""
GhcCanonVersion = $(GhcMajVersion)0$(GhcMinVersion)
else
GhcCanonVersion = $(GhcMajVersion)$(GhcMinVersion)
endif

HBC		= /usr/local/bin/hbc
NHC		= 

HC	        = /usr/bin/ghc
MKDEPENDHS	= $(GHC)

# Sometimes we want to invoke ghc from the build tree in different
# projects (eg. it's handy to have a nofib & a ghc build in the same
# tree).  We can refer to "this ghc" as $(GHC_INPLACE):

GHC_INPLACE 	= $(FPTOOLS_TOP)/ghc/compiler/ghc-inplace
GHC_STAGE1 	= $(FPTOOLS_TOP)/ghc/compiler/stage1/ghc-inplace
GHC_STAGE2 	= $(FPTOOLS_TOP)/ghc/compiler/stage2/ghc-inplace
GHC_STAGE3 	= $(FPTOOLS_TOP)/ghc/compiler/stage3/ghc-inplace

#-----------------------------------------------------------------------------
# C compiler
#
# NB. Don't override $(WhatGccIsCalled) using build.mk,  re-configure using
# the flag --with-gcc=<blah> instead.  The reason is that the configure script
# needs to know which gcc you're using in order to perform its tests.

HaveGcc 	= YES
UseGcc  	= YES
WhatGccIsCalled = gcc
ifeq "$(strip $(HaveGcc))" "YES"
ifneq "$(strip $(UseGcc))"  "YES"
  CC	= cc
else
  CC	= $(WhatGccIsCalled)
endif
endif

# default C compiler flags
SRC_CC_OPTS = -O

ifeq "$(TARGETPLATFORM)" "ia64-unknown-linux"
SRC_CC_OPTS += -G0
endif

# Solaris2 strikes again.
unix_SRC_HSC2HS_OPTS += 

#-----------------------------------------------------------------------------
# GMP Library (version 2.0.x or above)
#
HaveLibGmp	= YES
LibGmp		= gmp

#-----------------------------------------------------------------------------
# Mingwex Library
#
HaveLibMingwEx	= 

#-----------------------------------------------------------------------------
# HaskellSupport framework (Mac OS X)
#
HaveFrameworkHaskellSupport = 

#-----------------------------------------------------------------------------
# Regex library
# (if present in libc use that one, otherwise use the one in the tree)
#
HavePosixRegex  = 

#-----------------------------------------------------------------------------
# GTK+

GTK_CONFIG		= 

#-----------------------------------------------------------------------------
# Flex (currently unused, could be moved to glafp-utils)

# FLEX			= @LEX@
# Don't bother with -lfl, we define our own yywrap()s anyway.
# FLEX_LIB		= 
#WAS:FLEX_LIB		= @LEXLIB@

#-----------------------------------------------------------------------------
# Other standard (ha!) Unix utilities

AR			= /usr/bin/ar q
ArSupportsInput		= 
# Yuckage: for ghc/utils/parallel -- todo: nuke this dependency!!
BASH                    = /usr/local/bin/bash

CONTEXT_DIFF		= diff -C 1
CP			= cp
CPP			= gcc -E 
CTAGS 			= $(ETAGS)
#
# RAWCPP_FLAGS are the flags to give to cpp (viz, gcc -E) to persuade it to
# behave plausibly on Haskell sources.
#
RAWCPP_FLAGS            = -undef -traditional
FIND			= /usr/bin/find
INSTALL			= /usr/bin/install -c
#
# Sigh - the autoconf macro for INSTALL will subst a relative path to the fallback
# install-sh script (if chosen). This not terribly useful to us, so we convert
# it into an abs. path.
# 
INSTALL			:= $(subst .././install-sh,$(FPTOOLS_TOP_ABS)/install-sh,$(INSTALL))
LATEX			= latex
HEVEA			= hevea
HACHA			= hacha
LN_S			= ln -s
MANMACROS		= -man
MSMACROS 		= -ms
MV			= mv
NROFF			= nroff
PERL 			= /usr/bin/perl
PYTHON			= /usr/bin/python
PIC			= pic
PREPROCESSCMD		= $(CC) -E
RANLIB			= :
RM			= rm -f
SED			= /bin/sed
SHELL			= /bin/sh

# Some ld's support the -x flag and some don't, so the configure
# script detects which we have and sets LdXFlag to "-x" or ""
# respectively.
LD			= /usr/bin/ld
LD_X			= -x

#
# In emergency situations, REAL_SHELL is used to perform shell commands
# from within the ghc driver script, by scribbling the command line to
# a temp file and then having $(REAL_SHELL) execute it. 
#
# The reason for having to do this is that overly long command lines
# cause unnecessary trouble with some shells (e.g., /bin/sh on Solaris
# 2.5.1), which is why this backdoor is provided. The situation of overly
# long command lines is either encountered while doing `make boot' in ghc/compiler, 
# or when linking the compiler binary (`hsc'). 
#
# We do not use SHELL to execute long commands, as `make' will more than likely
# override whatever setting you have in your environment while executing. 

# By default, REAL_SHELL is set equal to SHELL, which is not really a smart move
# as it is SHELL that will show up the bogosity in the first place, but setting
# it to anything else isn't really portable.
#
#  ====> If long command lines cause you trouble, invoke `ghc' (via `make' or otherwise)
# with REAL_SHELL set to something else than /bin/sh, for instance, your favourite
# command shell.
#
REAL_SHELL=$(SHELL)
SIZE			= size
STRIP			= strip
TAR			= /bin/gtar
ZIP			= zip

#
# This is special to literate/, ToDo: add literate-specific
# configure setup to literate/.
#
TBL 			= tbl
TEX			= tex
TGRIND 			= tgrind
TGRIND_HELPER 		= /usr/local/lib/tgrind/tfontedpr # XXX
TIB			= tib

TIME			= /usr/bin/time
TROFF			= troff
UNAME			= uname

#-----------------------------------------------------------------------------
# SGML stuff

JADE			= /usr/bin/openjade

SGML2DVI		= $(DOCBOOK_PREFIX)db2dvi
SGML2HTML		= $(DOCBOOK_PREFIX)db2html
SGML2PS			= $(DOCBOOK_PREFIX)db2ps
SGML2PDF		= $(DOCBOOK_PREFIX)db2pdf
SGML2RTF		= $(DOCBOOK_PREFIX)db2rtf

SGMLSTYLESHEET          = $(FPTOOLS_TOP_ABS)/docs/fptools-both.dsl

SRC_SGML2DVI_OPTS       = -d $(SGMLSTYLESHEET)
SRC_SGML2HTML_OPTS      = -d $(SGMLSTYLESHEET)
SRC_SGML2PS_OPTS      	= -d $(SGMLSTYLESHEET)
SRC_SGML2RTF_OPTS       = -d $(SGMLSTYLESHEET)
SRC_SGML2PDF_OPTS       = -d $(SGMLSTYLESHEET)

DOCBOOK_CATALOG		= /etc/sgml/catalog

#-----------------------------------------------------------------------------
# 		FPtools support software

# Stuff from fptools/glafp-utils
MKDEPENDC 		= $(MKDEPENDC_PREFIX)mkdependC
LTX 			= $(LTX_PREFIX)ltx
MKDIRHIER		= $(MKDIRHIER_PREFIX)mkdirhier
LNDIR			= $(LNDIR_PREFIX)lndir
ETAGS			= $(ETAGS_PREFIX)etags
VERBATIM		= $(VERBATIM_PREFIX)verbatim
SGMLVERB		= $(SGMLVERB_PREFIX)sgmlverb
RUNTEST			= $(RUNTEST_PREFIX)runstdtest
LX			= @LxCmd@

BLD_DLL			= dllwrap

#
# .NET support software
#
ILX2IL                  = ilx2il
ILASM                   = ilasm

#
# ghc-pkg
#
GHC_PKG		        = /usr/bin/ghc-pkg

#
# GreenCard
#
GREENCARD	        = 
GREENCARD_VERSION	= 		

#
# Happy
#
HAPPY			= /usr/bin/happy
HAPPY_VERSION		= 1.14		
#
# Options to pass to Happy when we're going to compile the output with GHC
#
GHC_HAPPY_OPTS		= -agc

#
# Alex
#
ALEX			= /usr/local/bin/alex
ALEX_VERSION		= 		
#
# Options to pass to Happy when we're going to compile the output with GHC
#
GHC_ALEX_OPTS		= -g

#
# Haddock
# 
HADDOCK			= 
HADDOCK_INPLACE		= $(HADDOCK_PREFIX)/src/haddock-inplace

#
# Stuff from fptools/literate
#
LIT2PGM 		= $(LITERATE_PREFIX)lit2pgm
LIT2HTML      		= $(LITERATE_PREFIX)lit2html
LIT2LATEX     		= $(LITERATE_PREFIX)lit2latex
MKDEPENDLIT   		= $(LITERATE_PREFIX)mkdependlit
LIT2CHANGELOG 		= $(LITERATE_PREFIX)lit2changelog
LIT2TEXT 		= $(LITERATE_PREFIX)lit2text

#
# Stuff from fptools/ghc/utils
#
UNLIT	 		= $(UNLIT_PREFIX)unlit$(exeext)
HP2PS			= $(HP2PS_PREFIX)hp2ps$(exeext)
HSTAGS			= $(HSTAGS_PREFIX)hasktags
HSC2HS			= $(HSC2HS_PREFIX)hsc2hs-inplace

#
# Options for the compiling different `ways'. Various projects within
# the glorious fptools tree support building in various user-configured
# ways. For instance, you could set up one `way' such that the prelude
# libraries all were built with the option -ffoldr-build-on.
# 
# To configure up your own way, have a look at some of the standard ways
# such as profiling, and create your own set of WAY_*_OPTS defs below.
# After having done that, add your way string to WAYS, and after having
# run the configure script, the different projects will add the new way
# to the list of ways they support.
#

#
# IMPORTANT! The WAYS variable configures the different `ways'
# you want to build a project (or maybe just parts of it, as is
# the case for ghc/). This variable is intended set inside the
# project mk setup, enforcing a global fptools WAYS is a bit too
# much (do you *really* want to build glafp-utils the profiled-concurrent 
# way?)
#

#
# Definitions of the different ways:
#   
#   * their name:
#          - tag, e.g., p
#          - description, e.g., profiling
#   * what they mean to the driver:
#          - WAY_p_HC_OPTS gives the list of command-line options
#            to the driver.
#

#
# The ways currently defined.
#
ALL_WAYS=p t u s mp mg a b c d e f g h i j k l m n o A B
USER_WAYS=a b c d e f g h j k l m n o A B

#
# The following ways currently have treated specially, p u t mg,
# as the driver script treats these guys specially and needs to carefully be told
# about the options for these. Hence, we hide the required command line options
# for these in the ghc/driver, as this is the only place they are needed.
# 
# If you want to add to these default options, fill in the variables below:

# Way 'i':
WAY_i_NAME=ILX
WAY_i_HC_OPTS= -filx -fruntime-types

# Way 'p':
WAY_p_NAME=profiling
WAY_p_HC_OPTS= -prof

# Way 't':
WAY_t_NAME=ticky-ticky profiling
WAY_t_HC_OPTS= -ticky

# Way `u':
WAY_u_NAME=unregisterized (using portable C only)
WAY_u_HC_OPTS=-unreg

# Way `s':
WAY_s_NAME=threads (for SMP)
WAY_s_HC_OPTS=-smp

# Way `mp': 
WAY_mp_NAME=parallel
WAY_mp_HC_OPTS=-parallel

# Way `mg': 
WAY_mg_NAME=GranSim
WAY_mg_HC_OPTS=-gransim

#
# Add user-way configurations here:
#
WAY_A_NAME=
WAY_A_HC_OPTS=

WAY_B_NAME=
WAY_B_HC_OPTS=

WAY_a_NAME=
WAY_a_HC_OPTS=

WAY_b_NAME=
WAY_b_HC_OPTS=

WAY_c_NAME=
WAY_c_HC_OPTS=

WAY_d_NAME=
WAY_d_HC_OPTS=

WAY_e_NAME=
WAY_e_HC_OPTS=

WAY_f_NAME=
WAY_f_HC_OPTS=

WAY_g_NAME=
WAY_g_HC_OPTS=

WAY_h_NAME=
WAY_h_HC_OPTS=

WAY_j_NAME=
WAY_j_HC_OPTS=

WAY_k_NAME=
WAY_k_HC_OPTS=

WAY_l_NAME=
WAY_l_HC_OPTS=

WAY_m_NAME=
WAY_m_HC_OPTS=

WAY_n_NAME=
WAY_n_HC_OPTS=

WAY_o_NAME=
WAY_o_HC_OPTS=

################################################################################
#
#		31-bit-Int Core files
#
################################################################################

# 
# It is possible to configure the compiler and prelude to support 31-bit
# integers, suitable for a back-end and RTS using a tag bit on a 32-bit
# architecture.  Currently the only useful output from this option is external Core
# files.  The following additions to your build.mk will produce the
# 31-bit core output.  Note that this is *not* just a library "way"; the
# compiler must be built a special way too.

# GhcCppOpts +=-DWORD_SIZE_IN_BITS=31
# GhcLibHcOpts +=-fext-core -fno-code -DWORD_SIZE_IN_BITS=31
# GhcLibCppOpts += -DWORD_SIZE_IN_BITS=31
# SplitObjs=NO

