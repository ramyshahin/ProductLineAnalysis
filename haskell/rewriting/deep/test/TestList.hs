{-# LANGUAGE NoImplicitPrelude #-}
module TestList where
{-
import qualified Prelude as P

type Int = P.Int
(==) = (P.==)
(+)  = (P.+)

data List a = Nil | Cons a (List a)

head :: List a -> a
head (Cons x xs) = x

tail :: List a -> List a
tail Nil = Nil
tail (Cons x xs) = xs

length :: List a -> Int
length xs = if (xs == Nil) then 0 else 1 + length (tail xs)
-}