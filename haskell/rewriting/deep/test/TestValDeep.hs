-- TestVal.hs
-- Some simple tests for deep lifting of values

module TestValDeep where
    import SPL

x :: Var Int
x = (mkVarT 7)

foo :: Var [a] -> Var a
foo = head

