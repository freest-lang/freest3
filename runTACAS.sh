# docker run --rm --interactive -v $PWD:/app/ alcides:bisim bash runTACAS.sh

rm -rf run_*.log

for (( t = 0; t < 10000; t++ )); do
   for (( i = 0; i < 8; i++ )); do
      runhaskell -isrc -itest/ test/TACAS2020/TestTACAS.hs $t $i $t >> run_$i.log &
   done
   wait
done
