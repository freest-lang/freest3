# docker run --rm --interactive -v $PWD:/app/ alcides:bisim bash runTACAS.sh

rm -rf run_*.log

# ghc --make -isrc -itest/ test/TACAS2020/TestTACAS.hs
stack build TACAS2020-TEST

# file=TEST.log
# CONTENTS=`cat TEST.log`
j=0

while mapfile -t -n 3 ary && ((${#ary[@]})); do
    # printf '%s\n' "${ary[0]}"
    # printf -- '------\n'
    # printf '%s\n' "${ary[1]}"
    # printf -- '--- SNIP ---\n'
    #    printf '%d' '--- SNIP ---\n'
    for (( i = 0; i < 2; i++ )); do
      stack exec TACAS2020-TEST "${ary[0]}" "${ary[1]}" $i True >> run_positives_$i.log &
    done
    wait
    echo -en "\r$j"
    ((j++))
 done < positive_tests_10-20.log

# for (( t = 0; t < 1000; t++ )); do
#     for (( i = 0; i < 1; i++ )); do
# 		echo "1"
#     stack exec TACAS2020-TEST $t $i 1000000 True >> run_positives_$i.log &
#     done
#     wait
# done

# for (( t = 0; t < 1000; t++ )); do
#    for (( i = 0; i < 8; i++ )); do
#       ./test/TACAS2020/TestTACAS $t $i 1000000 False >> run_negatives_$i.log &
#    done
#    wait
# done


# generate tests

# for (( i = 0; i < 100; i++ )); do
#   stack exec TACAS2020-TEST 0 0 1000000 True >> positive_tests_0-10.log
#   echo -en "\r$i"  
# done


# while read LINE
# 	do
# 	    if [ "$LINE" != "" ]; then            
# 		echo -en $LINE"\n ------ \n"
# 		i = $i + 1
# 	    fi
# 	done < TEST.log
