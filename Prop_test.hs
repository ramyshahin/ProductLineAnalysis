-- Propositional Logic and SAT solving
-- Ramy Shahin
-- Jan 3rd 2017
module Prop where

import Z3.Monad
import qualified Data.Traversable as T

script :: Z3 Result
script = do
    p <- mkFreshBoolVar "p"
    q <- mkFreshBoolVar "q"
    r <- mkFreshBoolVar "r"
    s <- mkFreshBoolVar "s"

    assert =<< mkAnd =<< T.sequence [(mkNot q), (return q)]
    assert =<< mkOr =<< T.sequence [(mkNot p), (mkNot s)]

    check
    
run :: IO ()
run = do
    result <- evalZ3 script
    print result

