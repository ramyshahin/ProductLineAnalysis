module HelloRefactor where

import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.Refactor

import Control.Reference ((.-))
import Debug.Trace (trace)
import SrcLoc (RealSrcSpan)

tryItOut :: String -> String -> IO ()
tryItOut = tryRefactor (localRefactoring . dollarApp)

dollarApp :: RealSrcSpan -> LocalRefactoring
dollarApp sp = return . (nodesContained sp .- replaceExpr)

replaceExpr :: Expr -> Expr
replaceExpr (App fun (Paren arg)) = mkInfixApp fun (mkUnqualOp "$") arg
replaceExpr e = e 


