module VBool where

import SPL

{-
    data Bool =
        True
      | False
-}

data I_VBool =
    VTrue | 
    VFalse

type VBool = V I_VBool

{-
    cond :: Bool -> a -> a -> a
    cond b x y = 
        case b of
            True => x
            False => y 
-}

i_cond :: PresenceCondition -> I_VBool -> V a -> V a -> V a 
i_cond pc b x y =
    case b of
        VTrue -> x -- /^ pc
        VFalse -> y -- /^ pc 

--vcond :: VBool -> V a -> V a -> V a
--vcond c x y = (v i_cond) <*> c <*> x <*> y
    --map (\(v,pc) -> i_cond pc v x y) b