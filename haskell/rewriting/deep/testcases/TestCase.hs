-- TestCase.hs
-- Some simple tests for deep lifting of pattern matching

module TestCase where
import Data.List

foo :: Maybe Int -> Int
foo i = case i of
    Just v -> v * 3
    Nothing -> 0
