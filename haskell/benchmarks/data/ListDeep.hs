module ListDeep where
import SPL
import VPreludeDeep
import Debug.Trace

data List a = Nil | Cns (Var a) (List a)

type (List_ a) = Var (List a)
_Nil = mkVarT $ Nil
_Cns = mkVarT $ Cns
len :: List_ a -> Int_
len l  = let case0 __cntxt__ = (0 ^| __cntxt__)
             split0 __dummy__ = case __dummy__ of Nil -> ()
             case1 __cntxt__ x xs = ((+) ^| __cntxt__) <*> ((1 ^| __cntxt__)) <*> (len (xs /^ __cntxt__))
             split1 __dummy__ = case __dummy__ of Cns x xs -> (x, xs) in liftedCase (l) (\__dummy__ -> case __dummy__ of Nil -> 0
                                                                                                                         Cns x xs -> 1) [\__cntxt__ -> (uncurry0 (case0 __cntxt__)) . (liftV split0), \__cntxt__ -> (uncurry2 (case1 __cntxt__)) . (liftV split1)]

