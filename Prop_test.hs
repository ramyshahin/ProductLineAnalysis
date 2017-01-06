-- Propositional Logic and SAT solving Tests
-- Ramy Shahin
-- Jan 3rd 2017
import Prop

import GHC.Arr

u :: Universe
u = mkUniverse ["P", "Q", "R", "S"]

p, q, r, s :: Prop
p = Atom u 0
q = Atom u 1
r = Atom u 2
s = Atom u 3

prop1 = Conj [p, (Not q)]   -- sat
prop2 = Disj [(Not r), s]   -- sat
prop3 = Conj [p, (Not p)]   -- unsat
prop4 = Disj [r]            -- sat
prop5 = Conj [F, p, q]      -- unsat
prop6 = Disj [T, prop1, prop3] -- sat

props = [prop1, prop2, prop3, prop4, prop5, prop6]

run :: IO ()
run = mapM_ (\p -> do
                    r <- (checkSAT u p)
                    putStrLn ((show p) ++ "------>" ++ (show r))
            ) props

