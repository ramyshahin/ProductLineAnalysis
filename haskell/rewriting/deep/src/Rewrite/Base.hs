module Rewrite.Base where

import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Ann 
import Language.Haskell.Tools.Rewrite.Match.Decls

import Control.Reference -- ((.-), (.=), (^.) (&))
import FastString
import Debug.Trace 
import qualified Data.Set as S 
import qualified SPL as L

cntxtVar = "__cntxt__"
dummyVar = "__dummy__"

mkV :: Expr -> Expr
mkV e = mkParen (mkInfixApp e upOp $ cntxtExpr)

vCntxt      = "__cntxt__"
vTT         = "ttPC"
cntxtPat    = mkVarPat (mkName vCntxt)
cntxtExpr   = mkVar $ mkName vCntxt
ttExpr      = mkVar $ mkName vTT
restrictOp  = mkUnqualOp "/^"
upOp        = mkUnqualOp "^|"
--notSupported :: a -> a
notSupported x = trace ("Not supported: " ++ prettyPrint x) x

type Declarations = S.Set String

innerName :: Name -> Name
innerName n = n 

liftedTypeName :: Name -> Name
liftedTypeName n = mkName (prettyPrint n ++ "_")

consName :: Name -> Name
consName n = mkName ("C_" ++ prettyPrint n)

mkVarTOp = mkVar (mkName "mkVarT")
compOp   = mkUnqualOp "."
dollarOp = mkUnqualOp "$"

liftedCond = mkVar (mkName "liftedCond")
liftedNeg = mkVar (mkName "neg\'")
liftedCase = mkVar (mkName "liftedCase")

getLiftedPrimitiveOp :: String -> Operator
getLiftedPrimitiveOp o = mkUnqualOp ("^" ++ o)

isPrimitiveOp :: String -> Bool
isPrimitiveOp n = S.member n L.primitiveOpNames

isPrimitiveFunc :: String -> Bool
isPrimitiveFunc f =  S.member f L.primitiveFuncNames

isPrimitive :: String -> Bool
isPrimitive s = isPrimitiveOp s || isPrimitiveFunc s

debugDecls :: Declarations -> String
debugDecls ns = foldl (\r s -> r ++ " " ++ s) "Declarations: " $ ns

externalDecl :: Declarations -> Declarations -> Name -> Bool
externalDecl globals locals x = 
    let allDecls = S.union globals locals
        r = not $ S.member (prettyPrint x) allDecls
    in  trace (debugDecls allDecls ++ " External " ++ prettyPrint x ++ " ? " ++ show r) $ r
