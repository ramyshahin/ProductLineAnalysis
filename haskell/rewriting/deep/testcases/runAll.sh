TEST_FILES="
    Test1
	TestProd1
	TestSum1
    TestSum2
    TestSum3
    TestSum4
    "

./clean.sh

for f in $TEST_FILES
do
    ./run.sh "$f"
    diff $f'Deep.hs' $f'Deep.expected'
done
