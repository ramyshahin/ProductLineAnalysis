#!/bin/sh

FILES="plain oneFeat else"

for F in $FILES
do
	echo "Test case: " $F
	stack exec vcpp $F.c > $F.out
	diff $F.out $F.expected
done

