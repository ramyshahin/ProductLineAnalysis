module TestDeep where
import SPL
import Data.List
import Data.Map

x :: Var Int
x = (mkVarT 3)

y :: Var Int
y = (mkVarT 7)

z :: Var Int
z = (mkVarT 9)

foo :: Var Int -> Var Int -> Var Int -> Var Int
foo x y z = (mkVarT (+)) <*> (bar x y) <*> (baz z)

bar :: Var Int -> Var Int -> Var Int
bar x y = (mkVarT (+)) <*> x <*> y

baz :: Var Int -> Var Int
baz x = (mkVarT (\x -> -x)) <*> (x)

f = baz
g = baz

x' = f (f (g (mkVarT 2)))

xs :: [Var Int]
xs = [(mkVarT 3)]

