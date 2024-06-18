# docker run --rm --interactive -v $PWD:/app/ alcides:bisim bash runTACAS.sh

rm -rf run_*.log

# ghc --make -isrc -itest/ test/TACAS2020/TestTACAS.hs
stack build TACAS2020-TEST

j=0
lower=90
upper=100

# while mapfile -t -n 2 ary && ((${#ary[@]})); do
#     for (( i = 0; i < 16; i++ )); do
#      # printf '%s\n' "${ary[0]}"
#      # printf -- '------\n'
#      # printf '%s\n' "${ary[1]}"
#       stack exec TACAS2020-TEST True "${ary[0]}" "${ary[1]}" $i True >> run_positives_$i.log &
#      # printf -- '--- SNIP ---\n'
#     done
#     wait
#     ((j++))
#     echo -en "\r$j"    
# done < positives.txt
# # done < positive_tests_$lower-$upper.log

# echo ""

while mapfile -t -n 2 ary && ((${#ary[@]})); do
    for (( i = 0; i < 16; i++ )); do
      stack exec TACAS2020-TEST True "${ary[0]}" "${ary[1]}" $i False >> run_negatives_$i.log &
    done
    wait
    ((j++))
    echo -en "\r$j"    
# done < negatives.txt
done < negative_tests_$lower-$upper.log
 

# generate tests

# for (( i = 0; i < 100; i++ )); do
# #  stack exec TACAS2020-TEST False $lower $upper 1000000 True >> positive_tests_$lower-$upper.log
#   timeout 240 stack exec TACAS2020-TEST False $lower $upper 1000000 False >> negative_tests_$lower-$upper.log
#   echo -en "\r$i"  
# done
