module List where
import Debug.Trace

data List a =
    Nil
  | Cns a (List a)

len :: List a -> Int
len l =
    case l of
        Nil -> 0
        Cns x xs -> 1 + len xs
