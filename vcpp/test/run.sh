#!/bin/sh

FILES="plain splice oneFeat else include1"

for F in $FILES
do
	echo "Test case: " $F
	stack exec vcpp $F.c > $F.out
	diff $F.out $F.expected
done

