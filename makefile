
testParserInvalid :
	runhaskell -isrc -itest/UnitTests/ test/UnitTests/Parse/TestParserInvalidSpec.hs

testKindingValid :
	runhaskell -isrc -itest/UnitTests/ test/UnitTests/Validation/TestKindingValidSpec.hs

testKindingInvalid :
	runhaskell -isrc -itest/UnitTests/  test/UnitTests/Validation/TestKindingInvalidSpec.hs

testEquivalenceValid :
	runhaskell -isrc -itest/UnitTests/ test/UnitTests/Equivalence/TestEquivalenceValidSpec.hs

testEquivalenceInvalid :
	runhaskell -isrc -itest/UnitTests/ test/UnitTests/Equivalence/TestEquivalenceInvalidSpec.hs

testBisimValid:
	runhaskell -isrc -itest/UnitTests/ test/UnitTests/Equivalence/TestBisimValidSpec.hs

testBisimInvalid:
	runhaskell -isrc -itest/UnitTests/ test/UnitTests/Equivalence/TestBisimInvalidSpec.hs

testGrammarValid:
	runhaskell -isrc -itest/UnitTests/ test/UnitTests/Equivalence/TestGrammarValidSpec.hs

testGrammarInvalid:
	runhaskell -isrc -itest/UnitTests/ test/UnitTests/Equivalence/TestGrammarInvalidSpec.hs

clean :
	rm `find ./ -name '*.o' -o -name '*.hi' -o -name '*.tix'` -r

cleanCompiled : 
	find ./test/Programs/ValidTests/ -type f  ! -name "*.*" -delete

cleanGen :
	rm test/Programs/ValidTests/*/*.hs 	  

cleanOuts :
	rm test/outputs/*

cleanAll : clean cleanOuts

coverage :
	./coverage

# LOGFILE=$(LOGPATH)/`date +'%y.%m.%d %H:%M:%S'`
TM=$(shell date '+%Y-%m-%d-%H:%M')
backup :
	tar -czf "../backups/backup-$(TM).tar" *

.PHONY: test clean coverage
