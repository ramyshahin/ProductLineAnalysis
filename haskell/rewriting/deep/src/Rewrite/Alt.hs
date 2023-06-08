module Rewrite.Alt where

import Rewrite.Base
import qualified Data.Set as S
--import Rewrite.Case
--import Rewrite.Expr
import Rewrite.Pattern

splitAlts :: Integer -> [Alt] -> [Alt]
splitAlts index as =
    case as of
        []                  -> []
        ((Alt p _ _) : as') -> 
            let rhs = mkCaseRhs $ mkLit (mkIntLit index)
            in  (mkAlt p rhs Nothing) : (splitAlts (index + 1) as')

