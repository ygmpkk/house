# RPM spec file for GHC					         -*-rpm-spec-*-
#
# Copyright [1998..2002] The GHC Team
#
# Thanks to Zoltan Vorosbaranyi <vbzoli@vbzo.li> for suggestions in 
# earlier versions and Pixel <pixel@mandrakesoft.com> for coding tips.
#
# This file is subject to the same free software license as GHC.

# Values for 6.2 is set by the `configure' script.  SNAP releases are 
# CVS snapshots.  Official releases should replace SNAP by an appropriate 
# release numbers (they are usually numbered starting from 1).

%define version    6.2
%define release    SNAP
%define prefix     /usr

Summary: Glasgow Haskell Compilation system
Name: ghc
Version: %{version}
Release: %{release}
Copyright: BSD style w/o adv. clause
Group: Development/Languages
Source: http://haskell.org/ghc/dist/%{version}/ghc-%{version}-src.tar.bz2
URL: http://haskell.org/ghc/
BuildRoot: /var/tmp/ghc-%{version}-%{release}-root
Requires: gmp-devel
Provides: haskell
BuildRequires: happy >= 1.13, ghc >= 4.08, haddock, docbook-utils

%description
Haskell is the standard purely functional programming language; the
current language version is Haskell 98, agreed in December 1998.

GHC is a state-of-the-art programming suite for Haskell.  Included is
an optimising compiler generating good code for a variety of
platforms, together with an interactive system for convenient, quick
development.  The distribution includes space and time profiling
facilities, a large collection of libraries, and support for various
language extensions, including concurrency, exceptions, and foreign
language interfaces (C, C++, whatever).

A wide variety of Haskell related resources (tutorials, libraries,
specifications, documentation, compilers, interpreters, references,
contact information, links to research groups) are available from the
Haskell home page at <http://www.haskell.org/>.

%package prof
Summary: Profiling libraries for GHC
Group: Development/Libraries
Requires: ghc = %{PACKAGE_VERSION}-%{release}

%description prof
Profiling libraries for Glorious Glasgow Haskell Compilation System (GHC).
They should be installed when GHC's profiling subsystem is needed.

%package doc
Summary: Documentation for GHC
Group: Documentation

%description doc
Preformatted documentation for the Glorious Glasgow Haskell Compilation System 
(GHC) and its libraries.  It should be installed if you like to have local
access to the documentation in PostScript and HTML format.  Alternatively,
the documentation is available online at

  http://haskell.org/ghc/documentation.html

%changelog

* Tue Aug 19 2003 Sven Panne
- use autoreconf

* Wed Sep 26 2001 Manuel Chakravarty
- small changes for 5.04

* Wed Sep 26 2001 Manuel Chakravarty
- split documentation off into a separate package
- adapt to new docbook setup in RH7.1

* Mon Apr 16 2001 Manuel Chakravarty
- revised for 5.00
- also runs autoconf automagically if no ./configure found

* Thu Jun 22 2000 Sven Panne
- removed explicit usage of hslibs/docs, it belongs to ghc/docs/set

* Sun Apr 23 2000 Manuel Chakravarty
- revised for ghc 4.07; added suggestions from Pixel <pixel@mandrakesoft.com>
- added profiling package

* Tue Dec 7 1999 Manuel Chakravarty
- version for use from CVS

* Thu Sep 16 1999 Manuel Chakravarty
- modified for GHC 4.04, patchlevel 1 (no more 62 tuple stuff); minimises use
  of patch files - instead emits a build.mk on-the-fly

* Sat Jul 31 1999 Manuel Chakravarty
- modified for GHC 4.04

* Wed Jun 30 1999 Manuel Chakravarty
- some more improvements from vbzoli

* Fri Feb 26 1999 Manuel Chakravarty
- modified for GHC 4.02

* Thu Dec 24 1998 Zoltan Vorosbaranyi 
- added BuildRoot
- files located in /usr/local/bin, /usr/local/lib moved to /usr/bin, /usr/lib

* Tue Jul 28 1998 Manuel Chakravarty
- original version

%prep
%setup

# generate our own `build.mk'
#
# * this is a kludge (is it still needed?)
#
cat >mk/build.mk <<END
GhcLibWays = p
SRC_HAPPY_OPTS += -agc
#GhcWithInterpreter=NO
GhcWithInterpreter=YES
END

# run autoheader and autoconf if necessary
#
test -f configure || autoreconf

%build
./configure --prefix=%{prefix}
make boot
make all ps html
 
%install
rm -rf $RPM_BUILD_ROOT

# FIXME: this is necessary due to brokenness in ghc-5.00/mk/target.mk
make prefix=$RPM_BUILD_ROOT%{prefix} install-dirs

make prefix=$RPM_BUILD_ROOT%{prefix} install

mkdir -p $RPM_BUILD_ROOT%{prefix}/share/doc/ghc-%{version}
cp ghc/ANNOUNCE ghc/README $RPM_BUILD_ROOT%{prefix}/share/doc/ghc-%{version}

make prefix=$RPM_BUILD_ROOT%{prefix} datadir=$RPM_BUILD_ROOT%{prefix}/share/doc/ghc-%{version} SGMLDocWays="html ps" install-docs

# generate the file list for lib/ _excluding_ all files needed for profiling
# only
#
# * generating file lists in a BUILD_ROOT spec is a bit tricky: the file list
#   has to contain complete paths, _but_ without the BUILD_ROOT, we also do
#   _not_ want have directory names in the list; furthermore, we have to make
#   sure that any leading / is removed from %{prefix}/lib, as find has to 
#   interpret the argument as a relative path; however, we have to include the
#   leading / again in the final file list (otherwise, rpm complains)
# * isn't there an easier way to do all this?
#
dir=`pwd`
cd $RPM_BUILD_ROOT
libdir=`echo %{prefix}/lib | sed 's|^/||'`
find $libdir ! -type d ! -name '*.p_hi' ! -name '*_p.a' -print | sed 's|^|/|'\
     >$dir/rpm-noprof-lib-files
find $libdir ! -type d \( -name '*.p_hi' -or -name '*_p.a' \) -print | sed 's|^|/|'\
     >$dir/rpm-prof-lib-files
cd $dir

%clean
rm -rf $RPM_BUILD_ROOT

%files -f rpm-noprof-lib-files
%defattr(-,root,root)
%doc %{prefix}/share/doc/ghc-%{version}/ANNOUNCE
%doc %{prefix}/share/doc/ghc-%{version}/README
%{prefix}/bin/*

%files prof -f rpm-prof-lib-files
%defattr(-,root,root)

%files doc
%defattr(-,root,root)
%doc %{prefix}/share/doc/ghc-%{version}/*.ps
%doc %{prefix}/share/doc/ghc-%{version}/html
