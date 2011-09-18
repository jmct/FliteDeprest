SOURCES=$(wildcard Flite/*.*hs)
EXAMPLES=$(subst .hs,.red,$(wildcard examples2/*.hs))
EGSRC=$(wildcard examples2/*.hs)
TEST=vanilla
LOGS=$(subst .hs,.log-$(TEST),$(wildcard examples2/*.hs))
T=`date +%s`

fl: $(SOURCES)
	ghc -O2 --make fl -XTypeSynonymInstances -XOverlappingInstances

trace: fl
	./fl -r examples/HigherOrder.hs 2>&1 | sed 's/\([fF]unc\)/\n\1/g' > trace.txt

examples2/%.red : examples/%.hs
	./fl -r $< > $@ || true

examples/%.red : examples/%.hs
	./fl -r $< > $@ || true

examples : $(EXAMPLES)

test : fl
	#for x in examples2/*.hs; do echo -n $$x && ( ./fl -r $$x > /dev/null 2>&1 ) && echo "	success" || echo "	failure" ; done
	for x in examples2/*.hs; do echo -n $$x && ( time ( ./fl -r $$x > /dev/null 2>&1 ) && echo success ) ; done

logs : fl $(LOGS)

examples2/%.log-$(TEST) : examples2/%.hs
	( echo "-- ${TEST} $< ---" && ( time ( ./fl -r $< 2>&1 | tail -1 ) && echo success ) || true ) >> $@ 2>&1

debug : clean
	ghci -fbreak-on-error fl.hs

clean :
	rm -f Flite/*.{hi,o} Flite/Parsec/*.{hi,o} fl

.PHONY : logs
