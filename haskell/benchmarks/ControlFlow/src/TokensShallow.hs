module TokensShallow where

import SPL
import PresenceCondition
import SuperCAST

type VToken = V (Maybe Token)
type ShTokens = V [Maybe Token]

vcons :: V (Maybe t -> [Maybe t] -> [Maybe t])
vcons = v (:)

len :: [Maybe t] -> Int
len xs =
    case xs of
        [] -> 0
        (Just x) : xs' -> 1 + len xs'
        Nothing : xs' -> len xs'

vlength :: V ([Maybe t] -> Int)
vlength = v len

mkVToken :: [(Token, PresenceCondition)] -> VToken
mkVToken xs = complementV Nothing (map (\(x,pc) -> (Just x, pc)) xs)

mkShList tokens = foldr (\x xs -> vcons <*> (mkVToken x) <*> xs) (v []) tokens

tokensShallow sh = vlength <*> sh