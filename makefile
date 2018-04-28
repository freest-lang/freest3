
testTypesValid :
	runhaskell -isrc -itest/UnitTests/ test/UnitTests/Types/TestTypesEqSpec.hs

testTypesInvalid :
	runhaskell -isrc -itest/UnitTests/ test/UnitTests/Types/TestEqInvalidSpec.hs

testShow :
	runhaskell -isrc -itest/UnitTests/ test/UnitTests/Types/TestShowValidSpec.hs

testParserValid :
	runhaskell -isrc -itest/UnitTests/ test/UnitTests/Types/TestParserValidSpec.hs

testParserInvalid :
	runhaskell -isrc -itest/UnitTests/ test/UnitTests/Types/TestParserInvalidSpec.hs

testKindingValid :
	runhaskell -isrc -itest/UnitTests/ test/UnitTests/Types/TestKindingValidSpec.hs

#testKindingInvalid :
#	runhaskell -isrc -itest/UnitTests/  test/UnitTests/Types/TestInvalidKindingSpec.hs
testEquivalenceValid :
	runhaskell -isrc -itest/UnitTests/ test/UnitTests/Types/TestEquivalenceValidSpec.hs

testEquivalenceInvalid :
	runhaskell -isrc -itest/UnitTests/ test/UnitTests/Types/TestEquivalenceInvalidSpec.hs

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
