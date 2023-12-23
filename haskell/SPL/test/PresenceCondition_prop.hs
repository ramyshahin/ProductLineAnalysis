-- Propositional Logic and SAT solving Tests
-- Ramy Shahin
-- Jan 3rd 2017
{-# LANGUAGE TemplateHaskell #-}
module PresenceCondition_prop where

import Test.QuickCheck.All
import Control.Applicative
import Debug.Trace
import PresenceCondition as PC
import Control.Monad.ST
import GHC.Arr

p, q, r, s :: PresenceCondition 
[p, q, r, s] = map mkFeature ["P", "Q", "R", "S"]

--p = Atom u 0
--q = Atom u 1
--r = Atom u 2
--s = Atom u 3

pc1 = p /\ (negPC q)
pc2 = (negPC r) \/ s
pc3 = p /\ (negPC p) 
pc4 = r
pc5 = noConfigs /\ p /\ q
pc6 = allConfigs \/ pc1 \/ pc3
pc7 = p `contains` (negPC p)

prop1 = (not . PC.empty) $ pc1  -- sat
prop2 = (not . PC.empty) $ pc2  -- sat
prop3 = (      PC.empty) $ pc3  -- unsat
prop4 = (not . PC.empty) $ pc4  -- sat
prop5 = (      PC.empty) $ pc5  -- unsat
prop6 = (not . PC.empty) $ pc6  -- sat
prop7 = (not . PC.empty) $ pc7  -- sat
--prop8 = iff  p (neg p)      -- unsat
--prop9 = impl p q            -- sat
--prop10 = iff p q            -- sat

props = [prop1, prop2, prop3, prop4, prop5, prop6, prop7] --, prop8, prop9, prop10]

return []
runPCTests = $quickCheckAll

{-
run :: IO ()
run = mapM_ (\p -> do
                    let r = sat p
                    putStrLn ((show p) ++ "------>" ++ (show r))
            ) props
-}