

test :
	runhaskell -isrc -itest -fforce-recomp test/Spec.hs

testParser :
		runhaskell -isrc -itest -fforce-recomp test/Types/ParseSpec.hs

testKinding :
	runhaskell -isrc -itest -fforce-recomp test/Types/KindingSpec.hs

clean :
	rm `find ./ -name '*.o' -o -name '*.hi' -o -name '*.tix'` -rf

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
