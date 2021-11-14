{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Gen where

import GHC.Generics
import PresenceCondition

class Lifted a where
  single   :: b -> a b
  mult     :: [(b,PresenceCondition)]  -> a b
  restrict :: PresenceCondition -> a b -> a b

{--  
  default single :: (Generic (a b), GLifted (Rep (a b))) => b -> a b
  single x     = to $ gsingle x 
  default mult :: (Generic (a b), GLifted (Rep (a b))) => [(b,PresenceCondition)] -> a b
  mult xs = to $ gmult xs
  default restrict :: (Generic (a b), GLifted (Rep (a b))) => 
            PresenceCondition -> a b -> a b
  restrict pc x = to $ grestrict pc (from x)

class GLifted f where
  gsingle   :: b -> PresenceCondition -> f (a b)
  gmult     :: [(b,PresenceCondition)] -> f (a b)
  grestrict :: PresenceCondition -> f a -> f a

instance GLifted U1 where
  gsingle x pc = U1 -- TODO: review
  gmult xs = U1     -- TODO: review
  grestrict pc U1 = U1

instance (GLifted a, GLifted b) => GLifted (a :*: b) where
  gsingle   x pc = gsingle x pc
  gmult     xs   = gmult xs
  grestrict pc (x :*: y) = (grestrict pc x) :*: (grestrict pc y)

instance (GLifted a, GLifted b) => GLifted (a :+: b) where
  gsingle   x pc = gsingle x pc
  gmult     xs   = gmult xs
  grestrict pc (L1 x) = L1 $ grestrict pc x
  grestrict pc (R1 x) = R1 $ grestrict pc x

instance (GLifted a) => GLifted (M1 i c a) where
  gsingle   x pc = gsingle x pc
  gmult     xs   = gmult xs
  grestrict pc (M1 x) = M1 $ grestrict pc x

instance (Lifted a) => GLifted (K1 i (a b)) where
  gsingle   x pc = gsingle x pc
  gmult     xs   = gmult xs
  grestrict pc (K1 x) = K1 $ restrict pc x
--}
