# docker run --rm --interactive -v $PWD:/app/ alcides:bisim bash runTACAS.sh

rm -rf run_*.log

ghc --make -isrc -itest/ test/TACAS2020/TestTACAS.hs

for (( t = 0; t < 1000; t++ )); do
   for (( i = 0; i < 8; i++ )); do
      ./test/TACAS2020/TestTACAS $t $i 1000000 True >> run_positives_$i.log &
   done
   wait
done

for (( t = 0; t < 1000; t++ )); do
   for (( i = 0; i < 8; i++ )); do
      ./test/TACAS2020/TestTACAS $t $i 1000000 False >> run_negatives_$i.log &
   done
   wait
done
