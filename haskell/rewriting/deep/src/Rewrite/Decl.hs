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
rewriteType :: Bool -> Type -> Type
rewriteType init t = case t of
    FunctionType a b    -> 
        if      init
        then    mkFunctionType (mkVarType (mkName "Context")) (rewriteType False t)
        else    mkFunctionType (rewriteType False a) (rewriteType False b)
    --InfixTypeApp l op r -> mkInfixTypeApp (rewriteType l) op (rewriteType r)
    ParenType t         -> mkParenType (rewriteType True t)
    -- TODO: handle other cases
    _ -> mkParenType $ mkTypeApp tyVar t

-- TODO
-- mkTypeSignature takes only one name, so a signature might map
-- to multiple declarations
rewriteTypeSig :: TypeSignature -> Decl
rewriteTypeSig (TypeSignature ns t) = 
    let n = head $ _annListElems ns
    in  mkTypeSigDecl $ mkTypeSignature n (rewriteType True t)

-- rewrite constructor declaration
rewriteConDecl :: ConDecl -> ConDecl
rewriteConDecl d = 
    case d of
        ConDecl n ts -> mkConDecl n $ 
                        (map (rewriteType False) (_annListElems ts))
        _ -> notSupported d

rewriteDeclHead :: Declarations -> DeclHead -> DeclHead
rewriteDeclHead decls dh =
    case dh of
        NameDeclHead n -> mkNameDeclHead (innerName n)
        ParenDeclHead  b -> mkParenDeclHead (rewriteDeclHead decls b)
        DeclHeadApp f op -> mkDeclHeadApp (rewriteDeclHead decls f) op
        InfixDeclHead l op r -> notSupported dh

getName :: DeclHead -> Name
getName dh =
    case dh of
        NameDeclHead n -> n
        ParenDeclHead  b -> getName b
        DeclHeadApp f op -> getName f
        InfixDeclHead l op r -> mkName ""

getConName :: ConDecl -> Name
getConName c =
    case c of
        ConDecl n ts -> n
        _ -> notSupported (mkName "")

liftConstructor :: Name -> Decl
liftConstructor n =
    mkValueBinding $ mkFunctionBind 
        [mkMatch (mkMatchLhs (consName n) []) 
          (mkUnguardedRhs (mkInfixApp mkVarTOp compOp (mkVar n)))
          Nothing] 