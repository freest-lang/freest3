#!/bin/bash

echo "Run times and memory allocated" > testsProf.prof
declare -r N=10
echo "Number of runs: $N" >> testsProf.prof

for i in $( seq 1 $N )
do
   echo "---------------------------------" >> testsProf.prof
   cabal test testEquiv
   cat testEquiv.prof | grep "total" >> testsProf.prof
done
