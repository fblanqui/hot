# HOT, a Higher-Order Termination prover
# See the COPYRIGHTS and LICENSE files.
#
# - Frederic Blanqui, 2011-08-15

TPDB := tpdb

default: hot

hot: FORCE
	$(MAKE) -f Makefile.hot

wanda:
	$(MAKE) -f Makefile.wanda

thor:
	$(MAKE) -f Makefile.thor

FORCE:

###############################################################################

hot-files:
	for f in `find . -name \*.xml`; do ../main -ohot -xtc $$f > $${f%.xml}.hot; done

###############################################################################

hot-dir:
	mkdir -p hot
	for f in `ls -1 *.hot | grep -v Makefile.hot | xargs`; do ../main -oxtc -with-arity -hot $$f > hot/$${f%.hot}.xml; done

###############################################################################

clean-hot:
	$(MAKE) -f Makefile.hot clean

clean-wanda:
	$(MAKE) -f Makefile.wanda clean

clean-thor:
	$(MAKE) -f Makefile.thor clean

###############################################################################

curry-dir: $(TPDB) hot
	cp -dr $(TPDB) hot curry
	for f in `find curry -name \*.xml`; do g=/tmp/`basename $$f .xml`.hot; ../main -ohot -xtc $$f > $$g; ../main -oxtc -curry -hot $$g > $$f; done

###############################################################################

log: log.csv

log.csv: FORCE
	echo "file,hot,wanda,thor" > $@
	for f in `find . -name \*.xml`; do g=$${f%.xml}; echo "$$g,`head -n 1 $$g.log`,`head -n 1 $$g.wanda`,`head -n 1 $$g.thor`" >> $@; done

#in open office: =331-NB.SI(B2:B332;".*MAYBE.*")

scores:
	@tail -n +2 log.csv | awk -f scores.awk

###############################################################################

maybe-hot:
	@(for f in `find . -name \*.log`; do case `head -n 1 $$f` in MAYBE) echo $$f;; esac; done) | sort | less

errors:
	@grep YES log.csv | grep NO || exit 0

only-hot:
	@grep 'YES,MAYBE,MAYBE' log.csv || exit 0
	@grep 'NO,MAYBE,MAYBE' log.csv || exit 0

only-wanda:
	@grep 'MAYBE,YES,MAYBE' log.csv || exit 0
	@grep 'MAYBE,NO,MAYBE' log.csv || exit 0

only-thor:
	@grep 'MAYBE,MAYBE,YES' log.csv || exit 0
	@grep 'MAYBE,MAYBE,NO' log.csv || exit 0

nobody:
	@grep 'MAYBE,MAYBE,MAYBE' log.csv || exit 0

curry-hot:
	for f in $(shell find curry -name \*.xml); do g=$${f/#curry\/}; g=$${g%.xml}.log; if test `head -n 1 $$g` != `head -n 1 curry/$$g`; then echo $$f; fi; done

curry-wanda:
	for f in $(shell find curry -name \*.xml); do g=$${f/#curry\/}; g=$${g%.xml}.wanda; if test `head -n 1 $$g` != `head -n 1 curry/$$g`; then echo $$f; fi; done

curry-thor:
	for f in $(shell find curry -name \*.xml); do g=$${f/#curry\/}; g=$${g%.xml}.thor; if test `head -n 1 $$g` != `head -n 1 curry/$$g`; then echo $$f; fi; done

###############################################################################

TPDB_FILES := `find tpdb -name \*.xml | sed -e 's|\.xml||g'`

tpdb: FORCE
	@tail -n +2 tpdb.csv | awk -f scores.awk

tpdb.csv: FORCE
	echo "file,hot,wanda,thor" > $@
	for f in $(TPDB_FILES); do echo "$$f,`head -n 1 $$f.log`,`head -n 1 $$f.wanda`,`head -n 1 $$f.thor`" >> $@; done

###############################################################################

COMP11a := average lambda5 merge noneating shuffle twice
COMP11b := apply counterex2 foldl iterative lambda2 length loopy ordrec plode prefixsum prenex process
COMP11c := 001 002 013 016 017 024 028 Ex5TermProof
COMP11d := BTreeMember Ex2_8_1ConstSubstFix Ex4MapList Ex5Folding Ex5Sorting Ex6_11 Ex7_9 Ex7OrdinalRec Ex7Sorting Ex9Maps Hamming
COMP11e := 24 26 34 36 3
COMP11f := 12 \#3.10 \#3.18 31 \#3.36 33 \#3.8 perfect2 perfect

COMP11 := $(COMP11a:%=Kop_11/%) $(COMP11b:%=Mixed_HO_10/%) \
	$(COMP11c:%=Uncurried_Applicative_11/AotoYamada_05__%) \
	$(COMP11d:%=Uncurried_Applicative_11/Applicative_05__%) \
	$(COMP11e:%=Uncurried_Applicative_11/Applicative_AG01_innermost__\#4.%) \
	$(COMP11f:%=Uncurried_Applicative_11/Applicative_first_order_05__%)

COMP11_FILES := $(COMP11:%=$(TPDB)/%)

comp11:
	@tail -n +2 comp11.csv | awk -f scores.awk

comp11.csv: FORCE
	echo "file,hot,wanda,thor" > $@
	for f in $(COMP11_FILES); do echo "$$f,`head -n 1 $$f.log`,`head -n 1 $$f.wanda`,`head -n 1 $$f.thor`" >> $@; done

check-comp11-files:
	@for f in $(COMP11_FILES); do if test ! -f $$f.xml; then echo $$f; fi; done

###############################################################################

COMP10a := applicative app counterex2 filter foldl hrsdif1 lambda2 prefixsum rec reverse sdu sort
COMP10 := $(COMP10a:%=Mixed_HO_10/%)

COMP10_FILES := $(COMP10:%=$(TPDB)/%)

comp10:
	@tail -n +2 comp10.csv | awk -f scores.awk

comp10.csv: FORCE
	echo "file,hot,wanda,thor" > $@
	for f in $(COMP10_FILES); do echo "$$f,`head -n 1 $$f.log`,`head -n 1 $$f.wanda`,`head -n 1 $$f.thor`" >> $@; done

check-comp10-files:
	@for f in $(COMP10_FILES); do if test ! -f $$f.xml; then echo $$f; fi; done
