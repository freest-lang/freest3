#/bin/bash

#DIR=tmp/valid
DIR=FreeST/test/Programs/ValidTests
DIR1=FreeST/test/Programs/InvalidTests
#DIR1=tmp/invalid
suc=0
suc1=0
fail=0
fail1=0

echo "Valid tests..."
echo ""
for file in $DIR/*/*.fst; do
    [ -e "$file" ] || continue
    stack run freest $file &> /dev/null # $file-res
    if [ $? != 1 ]; then
        echo $file": Success"
        ((suc++))
    else
        echo $file": Failed"
        ((fail++))
    fi
done

echo "Invalid tests..."
echo ""


for file in $DIR1/*/*.fst; do
    [ -e "$file" ] || continue
    stack run freest $file &> /dev/null # $file-res
    if [ $? != 1 ]; then
        echo $file": Success"
        ((suc1++))
    else
        echo $file": Failed"
        ((fail1++))
    fi   
done

nValid=`ls $DIR/*/*.fst | wc -l`
echo ""
echo $nValid" valid tests:"
echo $suc" succeeded"
echo $fail" failed"

nInvalid=`ls $DIR1/*/*.fst | wc -l`
echo ""
echo $nInvalid" invalid tests:"
echo $fail1" failed"
echo $suc1" succeeded"
