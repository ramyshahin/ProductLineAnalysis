module Rewrite.Decl where

import Rewrite.Common
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

import Rewrite.Base

-- | Rewrite declarations
--
rewriteType :: Type -> Type
rewriteType t = case t of
    FunctionType a b    -> mkFunctionType (rewriteType a) (rewriteType b)
    --InfixTypeApp l op r -> mkInfixTypeApp (rewriteType l) op (rewriteType r)
    ParenType t         -> mkParenType (rewriteType t)
    TupleType ts        -> mkTupleType (map rewriteType (_annListElems ts))
    ListType t          -> mkListType (rewriteType t)
    -- TODO: handle other cases
    _ -> mkParenType $ mkTypeApp tyVar t

-- TODO
-- mkTypeSignature takes only one name, so a signature might map
-- to multiple declarations
rewriteTypeSig :: TypeSignature -> Decl
rewriteTypeSig (TypeSignature ns t) = 
    let n = head $ _annListElems ns
    in  mkTypeSigDecl $ mkTypeSignature n (rewriteType t)

-- rewrite constructor declaration
rewriteConDecl :: ConDecl -> ConDecl
rewriteConDecl d = 
    case d of
        ConDecl n ts -> mkConDecl n $ 
                        (map rewriteType (_annListElems ts))
        _ -> notSupported d

rewriteDeclHead :: Declarations -> DeclHead -> DeclHead
rewriteDeclHead decls dh =
    case dh of
        NameDeclHead n -> mkNameDeclHead (innerName n)
        ParenDeclHead  b -> mkParenDeclHead (rewriteDeclHead decls b)
        DeclHeadApp f op -> mkDeclHeadApp (rewriteDeclHead decls f) op
        InfixDeclHead l op r -> notSupported dh

getTypeName :: DeclHead -> String
getTypeName dh =
    case dh of
        NameDeclHead n -> prettyPrint n
        ParenDeclHead  b -> getTypeName b
        DeclHeadApp f op -> "(" ++ (getTypeName f) ++ " " 
                            ++ (prettyPrint op) ++ ")"
        InfixDeclHead l op r -> ""

getConName :: ConDecl -> Name
getConName c =
    case c of
        ConDecl n ts -> n
        _ -> notSupported (mkName "")

liftConstructor :: Name -> Decl
liftConstructor n =
    mkValueBinding $ mkFunctionBind 
        [mkMatch (mkMatchLhs (consName n) []) 
          (mkUnguardedRhs (mkInfixApp mkVarTOp dollarOp (mkVar n)))
          Nothing] 