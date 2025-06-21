#!/bin/bash
MODULE=$1
echo Lifting ${MODULE}.hs

rm -f ${MODULE}Deep.hs
#stack exec --profile -- deep-rewriter-exe "${MODULE}" >> ${MODULE}Deep.hs +RTC -xs
stack exec -- deep-rewriter-exe "${MODULE}" >> ${MODULE}Deep.hs
stack ghc ${MODULE}Deep.hs #Main.hs
