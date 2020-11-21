# HOT, a Higher-Order Termination prover
# See the COPYRIGHTS and LICENSE files.
#
# - Frederic Blanqui, 2011-08-31

# command to call the prover
PROVER := ~/hot-svn/thor

# file extension for prover output
EXT := thor

###############################################################################

XML_FILES := $(shell find . -name \*.xml)

default: $(XML_FILES:%.xml=%.$(EXT))

%.$(EXT): %.xml
	@echo -n "$* : " && (ulimit -t 60; $(PROVER) $*.xml > $*.$(EXT) 2>&1) && head -n 1 $*.$(EXT)

%: %.xml
	(ulimit -t 60; $(PROVER) $*.xml > $*.$(EXT) 2>&1); less $*.$(EXT)

clean:
	rm -f $(XML_FILES:%.xml=%.$(EXT))
