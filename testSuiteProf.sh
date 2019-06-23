#!/bin/bash

echo "Run times and memory allocated" > testSuiteProf.prof
declare -r N=10
echo "Number of runs: $N" >> testSuiteProf.prof

for i in $( seq 1 $N )
do
   echo "---------------------------------" >> testSuiteProf.prof
   #cabal test testEquiv
   cabal test bissimEquiv
   cat testEquiv.prof | grep "total" >> testSuiteProf.prof
done
