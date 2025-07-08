#!/bin/bash
MODULE=$1
echo Lifting ${MODULE}.hs

rm -f V${MODULE}.hs
#stack exec --profile -- deep-rewriter-exe "${MODULE}" >> V${MODULE}.hs +RTC -xs
stack exec -- deep-rewriter-exe "${MODULE}" >> V${MODULE}.hs
stack ghc V${MODULE}.hs #Main.hs
diff V${MODULE}'.hs' V${MODULE}'.expected'
