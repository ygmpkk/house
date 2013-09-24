# -----------------------------------------------------------------------------

TOP=.
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------

SUBDIRS = base haskell98 haskell-src

# Set GHCBootLibs=YES from the command line to work with just the libraries
# needed to bootstrap GHC.
ifneq "$(GHCBootLibs)" "YES"
SUBDIRS += network parsec QuickCheck
endif

ifeq "$(GhcLibsWithReadline)" "YES"
SUBDIRS += readline
endif

ifeq "$(GhcLibsWithOpenGL)" "YES"
SUBDIRS += OpenGL
endif

ifeq "$(GhcLibsWithGLUT)" "YES"
SUBDIRS += GLUT
endif

ifeq "$(GhcLibsWithObjectIO)" "YES"
SUBDIRS += ObjectIO
endif

ifeq "$(GhcLibsWithUnix)" "YES"
SUBDIRS += unix
endif

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk

# -----------------------------------------------------------------------------
# Generating the combined contents/index pages for the library docs

HTML_DIR = html

# ATTENTION, incomprehensible shell stuff ahead: Automagically create the
# prologue for the combined index via a header, the package prologues (in
# alphabetical order of the packages) and a footer. Not very nice, but much
# better than redundancy or a strong coupling with the packages.
libraries.txt: libraries-header.txt libraries-footer.txt
	$(RM) $@
	( cat libraries-header.txt ; echo ; \
	for i in `for j in $(SUBDIRS) ; do echo $$j ; done | sort -f` ; do \
	   grep -w name $$i/package.conf.in | sed "s/^.*\"\(.*\)\".*$$/[@\1@]/"; \
	   grep -v '^ *$$' $$i/prologue.txt; \
	   echo; \
	done ; \
	cat libraries-footer.txt ; echo ) > $@

$(HTML_DIR)/index.html : libraries.txt
	@$(INSTALL_DIR) $(HTML_DIR)
	$(HADDOCK) --gen-index --gen-contents -o $(HTML_DIR) \
		$(HADDOCK_OPTS) \
		-t "Haskell Hierarchical Libraries" \
		-p libraries.txt \
		$(foreach pkg, $(SUBDIRS), \
		   --read-interface=$(pkg),$(pkg)/$(pkg).haddock)

html :: $(HTML_DIR)/index.html

install-docs :: $(HTML_DIR)/index.html
	@$(INSTALL_DIR) $(datadir)/html/libraries/
	@for i in $(HTML_DIR)/*; do \
	   echo $(INSTALL_DATA) $(INSTALL_OPTS) $$i $(datadir)/html/libraries; \
	   $(INSTALL_DATA) $(INSTALL_OPTS) $$i $(datadir)/html/libraries; \
	done
