module TokensDeep where

import SPL
import PresenceCondition
import SuperCAST
import ListDeep

--type VToken = V Token
type VTokens = VList Token

vnil :: VList t
vnil@(V vnil_xs) = v I_Nil

vcons :: [Val t] -> VList t -> VList t
vcons vs xs = 
    let v = mkSubV vs 
        f = footprint v
    in complementV (Proxy_List xs) [(I_Cons v xs, f)]


mkVList :: [Val t] -> VList t
mkVList tokens = foldr (\x xs -> vcons [x] xs) vnil tokens

tokensDeep sh = do
    let sh_len = len sh
    putStrLn (show sh_len)
    --return ()