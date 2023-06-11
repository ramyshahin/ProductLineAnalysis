
{-# LANGUAGE LambdaCase #-}

module Rewrite.Deep where

import Rewrite.Common
    
-- AST types: https://github.com/haskell-tools/haskell-tools/tree/master/src/ast/Language/Haskell/Tools/AST/Representation
    
import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Ann 
import Language.Haskell.Tools.Rewrite.Match.Decls

import Control.Reference -- ((.-), (.=), (^.) (&))
import FastString
import Debug.Trace
import Data.Char
import qualified Data.Set as S 
import qualified SPL as L

import Rewrite.Base
import Rewrite.Decl

deepRewrite :: RefactoringChoice
deepRewrite = ModuleRefactoring "DeepRewrite" (localRefactoring deep)
    
run :: String -> String -> IO ()
run = tryRefactor (localRefactoring . (\_ -> deep))
    
deep :: LocalRefactoring
deep mod = do
    let decls = getModuleDeclarations mod
    --let rwDecls = concatMap (rewriteDecl decls) (mod ^. modDecl & annList)
    return  $ filePragmas & annListElems .- rewritePragma 
            $ modHead     & annMaybe     .- rewriteHeader
            $ modImports                 .- rewriteImports
            $ modDecl     & annListElems .- (concatMap (rewriteDecl decls))
            $ mod 
    
getModuleDeclarations :: Module -> Declarations
getModuleDeclarations mod = do
    let decls = _annListElems $ mod ^. modDecl
        names = map (prettyPrint . getLHSName) decls
        s     = S.fromList names
    trace (debugDecls s) $ s

-- | Rewriting imports
--
imports xs = map snd (zipWithSeparators xs)
    
noImplicitPrelude :: FilePragma
noImplicitPrelude = mkLanguagePragma ["NoImplicitPrelude"]
rewritePragma :: [FilePragma] -> [FilePragma] 
rewritePragma ps = noImplicitPrelude : ps

rewriteHeader :: Maybe ModuleHead -> Maybe ModuleHead 
rewriteHeader header =
    case header of
        Nothing -> Nothing
        Just h  -> Just $ mhName .- (appendModName "Deep") $ h

moduleNamePrelude = mkModuleName "VPrelude"
importPrelude = mkImportDecl False False False Nothing moduleNamePrelude Nothing Nothing

rewriteImports :: ImportDeclList  -> ImportDeclList 
rewriteImports xs = (annListElems .= concat [[importSPL, importPrelude], (imports xs)]) xs 
--rewriteImports xs = (annListElems .= concat [[importSPL], (imports xs)]) xs 
    
{-
rewriteAlt :: Declarations -> Declarations -> (Integer, Alt) -> Expr
rewriteAlt globals locals (index, Alt p (CaseRhs e) _) =
    let vCase   = mkVar $ mkName ("case" ++ show index)
        vSplit  = mkVar $ mkName ("split" ++ show index)
        vLiftV  = mkVar $ mkName ("liftV")
        pVars   = getPatternVars p
        vUncurry = mkVar $ mkName ("uncurry" ++ show (length pVars))
        dotOp   = mkUnqualOp "."
        c       = mkParen $ mkApp vUncurry (mkParen (mkApp vCase cntxtExpr))
        s       = mkParen $ mkApp vLiftV vSplit
    in  mkLambda [cntxtPat] $ mkInfixApp c dotOp s
-}

{-
rewriteApp :: Bool -> Declarations -> Declarations -> Expr -> Expr
rewriteApp init globals locals (App fun arg) = 
    let fun' = case fun of 
                App _ _ -> rewriteApp True globals locals fun
                _       -> rewriteExpr globals locals fun
        arg' = case arg of
                App _ _ -> rewriteApp False globals locals arg
                _       -> rewriteExpr globals locals arg
    in  case fun of 
            Var n -> if     externalDecl globals locals n
                     then   if isPrimitiveFunc (prettyPrint n)
                            then mkApp (rewritePrimitiveFuncName (prettyPrint n)) arg'
                            else mkInfixApp fun' appOp arg'
                     else   if      init 
                            then    mkApp (mkApp fun' cntxtExpr) arg'
                            else    mkApp fun' arg'
            _      -> case fun' of 
                        App _ _ -> mkApp fun' arg'
                        InfixApp _ op _ -> mkInfixApp fun' appOp arg' 
                        _       -> mkInfixApp fun' appOp arg'
-}
-- lifting expressions 


