################################################################
# Makefile for Graphics library documentation
# 
# Targets are:
#   dvi, ps, psgz  requires LaTeX
#   html           requires latex2html
################################################################

# comes first for usual reasons

#default	:: all
default		:: dvi

################################################################
# Source files
################################################################

HERE = `pwd`

TEX_FILES += Graphics.tex
BIB_FILES += $(wildcard *.bib)

################################################################
# Manual dependencies
################################################################

# Graphics.dvi : Graphics.bbl

################################################################
# Derived files
################################################################

DVI_FILES  += $(addsuffix .dvi, $(basename $(TEX_FILES)))

PS_FILES   += $(addsuffix .ps,  $(basename $(DVI_FILES)))
PSGZ_FILES += $(addsuffix .ps.gz,$(basename $(PS_FILES)))

HTML_DIRS  += $(addprefix html/, $(basename $(TEX_FILES)))
HTML_FILES += $(addsuffix /index.html, $(HTML_DIRS))

html	:: $(HTML_FILES)
dvi	:: $(DVI_FILES)
ps	:: $(PS_FILES)
psgz	:: ps $(PSGZ_FILES)

all	:: html
all	:: dvi
all	:: ps
all	:: psgz

################################################################
# Rules
################################################################

%.bbl	: %.aux $(BIB_FILES)
	-bibtex $*

%.ps	: %.dvi
	  dvips $< -o $*.ps

%.gz	: %
	  gzip -c $< > $@

# Wild hacking to build in correct directory
%.dvi	: %.tex 
#	cd $(dir $<); env TEXINPUTS=".:$(TEXINPUTS):" latex $(notdir $<)
	cd $(dir $<); env TEXINPUTS=".:$(TEXINPUTS):" latex $(notdir $<)
	-bibtex $*
	cd $(dir $<); env TEXINPUTS=".:$(TEXINPUTS):" latex $(notdir $<)
	cd $(dir $<); env TEXINPUTS=".:$(TEXINPUTS):" latex $(notdir $<)

clean	::
	$(RM) *.aux *.log *.toc
	$(RM) $(DVI_FILES)
	$(RM) $(PS_FILES)
	$(RM) $(PSGZ_FILES)
	$(RM) -r $(HTML_DIRS)
veryclean:: clean
	$(RM) *~

html_dir :
	if [ ! -d $(HERE)/html ]; then mkdir $(HERE)/html; fi

# Putting $(HERE) in this rule is horrible - but latex2html is horrible.
html/%/index.html: %.tex html_dir
	latex2html -info "" -dir $(HERE)/html/$(basename $<) $(HERE)/$<

################################################################
# End of Makefile
################################################################
