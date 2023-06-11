#!/bin/bash
MODULE=$1
echo Lifting ${MODULE}.hs

rm -f ${MODULE}Deep.hs
stack run deep-rewriter-exe "${MODULE}" >> ${MODULE}Deep.hs
stack ghc ${MODULE}Deep.hs #Main.hs
