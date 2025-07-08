module List where
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

lmap :: (a -> b) -> List a -> List b
lmap f xs = 
    case xs of
        Nil -> Nil
        Cons y ys -> Cons (f y) (lmap f ys)

lfoldr :: (a -> b -> b) -> b -> List a -> b
lfoldr f i xs = 
    case xs of
        Nil -> i
        Cons y ys -> f y (lfoldr f i ys)

lfoldl :: (b -> a -> b) -> b -> List a -> b
lfoldl f i xs = 
    case xs of
        Nil -> i
        Cons y ys -> lfoldl f (f i y) ys

len :: List a -> Int
len xs = 
    case xs of
        Nil -> 0 
        Cons x xs' -> 1 + len xs'
