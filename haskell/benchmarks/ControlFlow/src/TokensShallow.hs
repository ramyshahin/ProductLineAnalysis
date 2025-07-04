module TokensShallow where

import SPL
import PresenceCondition
import SuperCAST

type VToken = V Token
type VTokens = V Tokens

vcons :: V (t -> [t] -> [t])
vcons = v (:)

vlength :: V ([t] -> Int)
vlength = v length

mkVToken :: [(Token, PresenceCondition)] -> VToken
mkVToken = complementV ""

tokensShallow tokens = do
    let sh  = foldr (\x xs -> vcons <*> (complementV "" x) <*> xs) (v []) tokens
    let sh_len = vlength <*> sh
    putStrLn (show sh_len)
