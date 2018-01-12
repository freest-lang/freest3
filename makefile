test :
	runhaskell -isrc -itest -fforce-recomp test/Spec.hs

testValidTypes :
	runhaskell -isrc -itest -fforce-recomp test/Types/TestValidTypesSpec.hs

testParser :
	runhaskell -isrc -itest -fforce-recomp test/Types/ParseSpec.hs

testInvalidParser :
	runhaskell -isrc -itest -fforce-recomp test/Types/ParseInvalidSpec.hs

testValidKinding :
	runhaskell -isrc -itest -fforce-recomp test/Types/TestValidKindingSpec.hs

#testInvalidKinding :
#	runhaskell -isrc -itest -fforce-recomp test/Types/TestInvalidKindingSpec.hs

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
