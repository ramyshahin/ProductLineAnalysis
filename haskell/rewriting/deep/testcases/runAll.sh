TEST_FILES="
    Test1
    "

./clean.sh

for f in $TEST_FILES
do
    ./run.sh "$f"
    diff $f'Deep.hs' $f'Deep.expected'
done
