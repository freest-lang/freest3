test :
	runhaskell -isrc -itest -fforce-recomp test/Spec.hs

testTypesValid :
	runhaskell -isrc -itest -fforce-recomp test/Types/TestTypesValidSpec.hs

testTypesInvalid :
	runhaskell -isrc -itest -fforce-recomp test/Types/TestTypesInvalidSpec.hs

testShow :
	runhaskell -isrc -itest -fforce-recomp test/Types/TestShowValidSpec.hs


testParserValid :
	runhaskell -isrc -itest -fforce-recomp test/Types/TestParserValidSpec.hs

testParserInvalid :
	runhaskell -isrc -itest -fforce-recomp test/Types/TestParserInvalidSpec.hs

testKindingValid :
	runhaskell -isrc -itest -fforce-recomp test/Types/TestKindingValidSpec.hs

#testKindingInvalid :
#	runhaskell -isrc -itest -fforce-recomp test/Types/TestInvalidKindingSpec.hs
testEquivalenceValid :
	runhaskell -isrc -itest -fforce-recomp test/Types/TestEquivalenceValidSpec.hs

testEquivalenceInvalid :
	runhaskell -isrc -itest -fforce-recomp test/Types/TestEquivalenceInvalidSpec.hs



clean :
	rm `find ./ -name '*.o' -o -name '*.hi' -o -name '*.tix'` -r

cleanOuts :
	rm test/outputs/*

cleanAll : clean cleanOuts

coverage :
	./testCoverage

# LOGFILE=$(LOGPATH)/`date +'%y.%m.%d %H:%M:%S'`
TM=$(shell date '+%Y-%m-%d-%H:%M')
backup :
	tar -czf "../backups/backup-$(TM).tar" *

.PHONY: test clean coverage
