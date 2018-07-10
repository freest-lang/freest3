
testEqValid :
	runhaskell -isrc -itest/UnitTests/ test/UnitTests/Syntax/TestEqValidSpec.hs

testEqInvalid :
	runhaskell -isrc -itest/UnitTests/ test/UnitTests/Syntax/TestEqInvalidSpec.hs

testShow :
	runhaskell -isrc -itest/UnitTests/ test/UnitTests/Syntax/TestShowValidSpec.hs

testParserValid :
	runhaskell -isrc -itest/UnitTests/ test/UnitTests/Parse/TestParserValidSpec.hs

testParserInvalid :
	runhaskell -isrc -itest/UnitTests/ test/UnitTests/Parse/TestParserInvalidSpec.hs

testKindingValid :
	runhaskell -isrc -itest/UnitTests/ test/UnitTests/Validation/TestKindingValidSpec.hs

testKindingInvalid :
	runhaskell -isrc -itest/UnitTests/  test/UnitTests/Validation/TestKindingInvalidSpec.hs

testEquivalenceValid :
	runhaskell -isrc -itest/UnitTests/ test/UnitTests/Validation/TestEquivalenceValidSpec.hs

testEquivalenceInvalid :
	runhaskell -isrc -itest/UnitTests/ test/UnitTests/Validation/TestEquivalenceInvalidSpec.hs

clean :
	rm `find ./ -name '*.o' -o -name '*.hi' -o -name '*.tix'` -r

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
