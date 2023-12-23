-- TestCase.hs
-- Some simple tests for deep lifting of pattern matching

module TestCaseDeep where
{-
import SPL
import Data.List

foo :: Var Maybe Int -> Var Int
foo i  = liftedCase (\caseJust v -> v * 3
                          Nothing -> 0) i

-}