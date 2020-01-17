# STACK ALIAS

units:
	stack test :units
programs:
	stack test :programs
valid-types-quick:
	stack test :valid-types-quick

#PROGRAMS
valid-programs:
	stack test :programs --ta "-m CompilerValid"
invalid-programs:
	stack test :programs --ta "-m CompilerInvalid"

# EQUIVALENT TYPES
equiv-types:
	stack test :units --ta "-m TestEquivalenceValid"
nonequiv-types:
	stack test :units --ta "-m TestEquivalenceInvalid"
equiv:
	stack test :units --ta "-m TestEquivalence"

# VALID TYPES
valid-types:
	stack test :units --ta "-m TestTypesValid"
invalid-types:
	stack test :units --ta "-m TestTypesInvalid"
types:
	stack test :units --ta "-m TestTypes"

#clean : rm `find ./ -name '*.o' -o -name '*.hi' -o -name '*.tix'` -r

#cleanCompiled : find ./test/Programs/ValidTests/ -type f  ! -name "*.*" -delete

#cleanGen : rm test/Programs/ValidTests/*/*.hs 	  

#cleanOuts : rm test/outputs/*

#cleanAll : clean cleanOuts

#coverage : ./coverage

# LOGFILE=$(LOGPATH)/`date +'%y.%m.%d %H:%M:%S'`
# TM=$(shell date '+%Y-%m-%d-%H:%M')
# backup :
# 	tar -czf "../backups/backup-$(TM).tar" *

#.PHONY: test clean coverage
