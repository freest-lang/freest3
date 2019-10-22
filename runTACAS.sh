# docker run --rm --interactive -v $PWD:/app/ alcides:bisim bash runTACAS.sh

rm -rf run_*.log

for (( t = 0; t < 5000; t++ )); do
   for (( i = 0; i < 8; i++ )); do
      runhaskell -isrc -itest/ test/TACAS2020/TestTACAS.hs $t $i 1000000 >> run_positives_$i.log &
   done
   wait
done

for (( t = 0; t < 5000; t++ )); do
   for (( i = 0; i < 8; i++ )); do
      runhaskell -isrc -itest/ test/TACAS2020/TestTACASNegative.hs $t $i 1000000 >> run_negatives_$i.log &
   done
   wait
done
