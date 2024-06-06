module TestSum1 where
import Data.List

-- testing ADTs
data MaybeInt =
   None
 | Some Int
 
-- testing recursive Algebraic types
--data ListInt =
--    Nil
--  | Cons Int ListInt

--head :: ListInt -> Int
--head xs = case xs of
--     Nil -> 0
--     Cons x xs -> x

--xs :: [Int]
--xs = [3]

-- testing conditionals
--c a b = if (a > b) then bar a b else b - a