-- PropBDD.hs
-- Ramy Shahin
-- May 7th 2017
module PropBDD where

import Data.Boolean.BF

type Prop = BF

mkUniverse :: [String] -> [Prop]
mkUniverse = map BFvar

