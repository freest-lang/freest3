#!/bin/bash

commandName=$1
fileNum=`ls Profiling | grep $commandName | wc -l`
fileName=$commandName"_"$fileNum$".prof"

echo "Run times and memory allocated" > $fileName
declare -r N=10
echo "Number of runs: $N" >> $fileName

for i in $( seq 1 $N )
do
   echo "---------------------------------" >> $fileName
     #cabal test testEquiv
   cabal test $commandName
   cat $commandName".prof" | grep "total" >> $fileName
done

mv $fileName Profiling/
