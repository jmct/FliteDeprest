SOURCES=$(wildcard Flite/*.*hs)
EXAMPLES=$(subst .hs,.red,$(wildcard examples/*.hs))


fl: $(SOURCES)
	ghc -O2 --make fl -XTypeSynonymInstances -XOverlappingInstances

trace: fl
	./fl -r examples/Lambda.hs 2>&1 | sed 's/\([fF]unc\)/\n\1/g' > trace.txt

examples/%.red : examples/%.hs
	./fl -r $< > examples/$@

examples : $(EXAMPLES)

test : fl
	for x in examples/*; do echo -n $$x && ( ./fl -r $$x > /dev/null 2>&1 ) && echo "	success" || echo "	failure" ; done

clean :
	rm -f Flite/*.{hi,o} Flite/Parsec/*.{hi,o} fl
