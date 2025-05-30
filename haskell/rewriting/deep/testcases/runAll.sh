TEST_FILES="
    TestBasic
    TestProd1
    TestSum1
    TestSum2
    TestSum3
    TestSum4
    TestSum5
    TestRecursive1
    TestRecursive2
    TestBool
    TestList
    "

./clean.sh

#stack build --profile deep 
for f in $TEST_FILES
do
    ./run.sh "$f"
    diff $f'Deep.hs' $f'Deep.expected'
done
