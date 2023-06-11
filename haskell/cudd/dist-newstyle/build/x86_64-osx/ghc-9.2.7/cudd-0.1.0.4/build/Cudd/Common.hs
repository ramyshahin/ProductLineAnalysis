{-# LINE 1 "Cudd/Common.hsc" #-}
module Cudd.Common (
    SatBit(..),
    toSatBit,
    expand,
    cudd_unique_slots,
    cudd_cache_slots
    ) where




data SatBit = Zero | One | DontCare deriving (Eq)

toSatBit :: Int -> SatBit
toSatBit 0 = Zero
toSatBit 1 = One
toSatBit 2 = DontCare
toSatBit _ = error "toSatBit: Invalid sat bit returned from CUDD"

expand :: SatBit -> [Bool]
expand Zero     = [False]
expand One      = [True]
expand DontCare = [False, True]

cudd_unique_slots :: Int
cudd_unique_slots = 256
{-# LINE 27 "Cudd/Common.hsc" #-}

cudd_cache_slots :: Int
cudd_cache_slots = 262144
{-# LINE 30 "Cudd/Common.hsc" #-}

