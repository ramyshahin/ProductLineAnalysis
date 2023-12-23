{-# LANGUAGE NoImplicitPrelude #-}
module ListDeep where
{-
import SPL
import qualified Prelude as P

type Int = Var P.Int
(==) = (P.==)
(+)  = (P.+)

data List a = Nil | Cons Var a (Var Var List Var a)

head :: Var Var List Var a -> Var a
head (Cons x xs) = x

tail :: Var Var List Var a -> Var Var List Var a
tail Nil = Nil
tail (Cons x xs) = xs

length :: Var Var List Var a -> Var Int
length xs = if ((mkVarT (==)) <*> xs <*> Nil) then (mkVarT 0) else (mkVarT (+)) <*> (mkVarT 1) <*> length (tail xs)
-}