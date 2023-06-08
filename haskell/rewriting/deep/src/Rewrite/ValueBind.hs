module Rewrite.ValueBind where

import Rewrite.Base
import Rewrite.Pattern
--import Rewrite.Match

import Control.Reference -- ((.-), (.=), (^.) (&))
import qualified Data.Set as S

getLocalVars :: LocalBind -> S.Set String
getLocalVars b =
    case b of
        LocalValBind vb -> getValBindVars vb
        _               -> trace ("Unhandled LocalBind " ++ prettyPrint b) $ S.empty

getLocalsVars :: LocalBinds -> S.Set String
getLocalsVars (LocalBinds bs) = foldl S.union S.empty $ map getLocalVars (_annListElems bs)

getBindLHSName :: ValueBind -> Name
getBindLHSName b = 
    case b of 
        SimpleBind (VarPat p) _ _ -> p
        FunctionBind ms ->  let  ms'   = _annListElems ms
                                 h     = head ms'
                                 match = h ^. matchLhs
                            in case match of 
                                MatchLhs n _ -> trace ("Decl: " ++ prettyPrint n) n
                                _ -> mkName ""

getValBindVars :: ValueBind -> S.Set String
getValBindVars vb =
    case vb of
        SimpleBind pat rhs _ -> getPatternVars pat
        _                    -> trace ("Unhandled FunBind " ++ prettyPrint vb) $ S.empty
 
