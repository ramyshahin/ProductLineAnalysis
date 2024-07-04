TEST_FILES="
    Test1
	TestProd1
	TestSum1
    TestSum2
    TestSum3
    TestSum4
    TestSum5
    TestRecursive1
    TestRecursive2
    TestBool
    "

./clean.sh

for f in $TEST_FILES
do
    ./run.sh "$f"
    diff $f'Deep.hs' $f'Deep.expected'
done
