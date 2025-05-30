module TestList where
import Data.Maybe

data List a = Nil | Cons a (List a)

head :: List a -> a
head xs = 
    case xs of
        Cons x xs -> x

tail :: List a -> List a
tail xs =
    case xs of 
        Nil -> Nil
        Cons x xs -> xs

len :: List a -> Int
len xs = 
    case xs of
        Nil -> 0 
        Cons x xs' -> 1 + len xs'
