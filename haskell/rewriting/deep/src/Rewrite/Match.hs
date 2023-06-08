module Rewrite.Match where

import Rewrite.Base
import Rewrite.Pattern
import Rewrite.Rhs

import qualified Data.Set as S 

getMatchVars :: Match -> S.Set String
getMatchVars (Match lhs _ _) =
    case lhs of 
        MatchLhs _ args -> foldl S.union S.empty $ map getPatternVars (_annListElems args)
        InfixLhs lhs _ rhs args -> 
            foldl S.union (S.union (getPatternVars lhs) (getPatternVars rhs)) $ map getPatternVars (_annListElems args)

