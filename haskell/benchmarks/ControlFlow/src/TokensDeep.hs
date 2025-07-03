module TokensDeep where

import SPL
import PresenceCondition
import SuperCAST
import ListDeep

--type VToken = V Token
type VTokens = VList Token

vnil :: VList t
vnil@(V vnil_xs) = v I_Nil

vcons :: (t, PresenceCondition) -> VList t -> VList t
vcons (x,pc) xs = complementV (Proxy_List xs) [(I_Cons (V [(x,pc)]) xs, pc)]

tokensDeep tokens = do
    let sh  = foldr (\x xs -> vcons x xs) vnil tokens
    let sh_len = len sh
    putStrLn (show sh_len)
    --return ()