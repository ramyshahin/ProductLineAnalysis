{-# LANGUAGE OverloadedStrings #-}

module CFGParser where

import CFG
import Language.C.Parser
import Language.C.Data.InputStream
import Language.C.Data.Position
import Language.C.Syntax.AST
import Debug.Trace
import Control.Exception
import qualified Data.Text.IO       as TIO
import qualified Data.Text          as T
import qualified Data.List          as L
import qualified Data.Maybe         as M
import qualified Data.MultiMap      as MM
import SPL
import PresenceCondition 

type VNode = Var CFGNode

dummyNode = CFGNode 0 T.empty (CFGDummy T.empty) [] []

mkV :: a -> (a, PCExpr) -> Var a
mkV dummy (v, pc) = 
    if   pc == ttPC
    then mkVarT v
    else mkVars [(v, pc), (dummy, negPC pc)]

mkVNode' = liftV5 CFGNode

lv2vl :: Show a => [Var a] -> Var [a]
lv2vl = foldr (|:|) (mkVarT [])

processEdge :: T.Text -> ((Int, Int), PCExpr)
processEdge t = --trace "processEdge" $
    let (e : from' : to' : pc' : []) = T.splitOn ";" t
        from = (read (T.unpack from')) :: Int
        to   = (read (T.unpack to'))   :: Int
        pc   = parsePC (T.unpack pc')
    in  assert (e == "E") $ ((from, to), pc)



processNode :: [((Int, Int), PCExpr)] -> T.Text -> Var CFGNode
processNode edges record = 
    let (n : id' : t : lineNum' : rest) = T.splitOn ";" record
        id          = (read (T.unpack id')) :: Int
        lineNum     = (read (T.unpack lineNum')) :: Int
        cCode       = T.intercalate ";" $ L.init rest
        pc          = parsePC $ T.unpack (L.last rest)
        (ast, pc')  = parseNode cCode lineNum t
        preds       = map (\((f,_), pc) -> mkV 0 (f,pc)) $ filter (\((_,t),_) -> t == id) edges
        succs       = map (\((_,t), pc) -> mkV 0 (t,pc)) $ filter (\((f,_),_) -> f == id) edges
        node        = mkVNode' (mkVar id (pc /\ pc')) (mkVarT cCode) (mkVarT ast) (lv2vl preds) (lv2vl succs)
    in  assert (n == "N") $
        assert (length rest >= 2) $ 
        fixCompleteness dummyNode node

readCFG :: String -> IO (Var CFG)
readCFG inputFileName = do
    fileTxt <- TIO.readFile inputFileName
    let lines = T.lines fileTxt
    let (nodeRecs, edgeRecs) = L.partition (\t -> T.head t == 'N') lines
    let edges = map processEdge edgeRecs
    let nodes = map (processNode edges) nodeRecs
    return $ mkCFG' (lv2vl nodes)

mkCFG :: [CFGNode] -> CFG
mkCFG  ns = CFG $ foldr (\n m -> MM.insert (_nID n) n m) MM.empty ns

mkCFG' = liftV mkCFG

-- parsing node expression/statement
parseDeclaration :: Int -> String -> NodeType
parseDeclaration lineNum s = --trace "parseDeclaration" $ 
    let input = inputStreamFromString s 
        p     = execParser_ extDeclP input (position 0 "" lineNum 0 Nothing)
    in  case p of
            Left e -> --if trace ("parseDeclaration failed: " ++ s) tryAppend 
                      --then appendDummy lineNum s 
                          --trace ("Dummy declaration node: " ++ s) $ 
                          CFGDummy (T.pack s)
            Right e -> CFGVarDecl e

--appendDummy :: Int -> String -> NodeType
--appendDummy lineNum s = 
--    let s' = s ++ "x += 0;"
--   in  parseStatement lineNum s' False

parseStatement :: Int -> String -> NodeType
parseStatement lineNum s = --trace ("parseStatement: " ++ s) $
    let input = inputStreamFromString (s ++ ";") 
        p     = execParser_ statementP input (position 0 "" lineNum 0 Nothing)
    in  case p of
            Left e -> --trace ("Dummy statement node: " ++ s) $ CFGDummy (T.pack s)
                parseExpr lineNum s
            Right e -> CFGStat e

parseExpr :: Int -> String -> NodeType 
parseExpr lineNum s = --trace "parseExpr" $
    let input = inputStreamFromString s 
        p     = execParser_ expressionP input (position 0 "" lineNum 0 Nothing)
    in  case p of
            Left e -> parseDeclaration lineNum (s ++ ";")
                --trace ("Dummy expression node: " ++ s) $ CFGDummy (T.pack s)
            Right e -> CFGExpr e

extractPC :: T.Text -> (T.Text, PCExpr)
extractPC t = 
    let i        = findMatchingParen 1 0 t
        (pc, t') = T.splitAt i t
    in  --trace (show t) $ trace ("PC : " ++ (show pc)) 
        (t', parsePC (T.unpack (T.strip pc)))
    where findMatchingParen i c t = 
            case T.head t of
                '(' -> findMatchingParen (i+1) (c+1) $ T.tail t
                ')' -> if c == 1 then i else findMatchingParen (i+1) (c-1) $ T.tail t
                _   -> findMatchingParen (i+1) c $ T.tail t

splitPCs :: T.Text -> [(T.Text, PCExpr)]
splitPCs t =
    let (c0, rest)  = T.breakOn "#if" t
        (c1, rest') = T.breakOn "#endif" (T.drop 3 rest)
    in  if T.null rest then [(c0, ttPC)] else (c0, ttPC) : (extractPC c1) : splitPCs (T.drop 6 rest')

parseNode :: T.Text -> Int -> T.Text -> (NodeType, PCExpr)
parseNode t lineNum nodeType = --trace "parseNode" $ 
    let (nodeText,pc)    = fixCFragment t
        ast              = case T.unpack nodeType of
                                "statement"         -> parseStatement lineNum (T.unpack nodeText)
                                "expression"        -> parseExpr lineNum (T.unpack nodeText)
                                "declaration"       -> CFGDecl $ T.strip nodeText
                                "function"          -> CFGFuncRoot $ T.strip nodeText
                                "function-inline"   -> CFGFunc $ T.strip nodeText
                                "function-static"   -> CFGFunc $ T.strip nodeText
                                _                   -> trace ("Unknown node type: " ++ T.unpack nodeType) $ CFGDummy nodeText
    in  (ast, pc)
    where fixCFragment s =  let (t0', t0'')             = T.breakOnEnd "::" s
                                t0                      = if T.null t0'' then t0' else T.dropEnd 2 t0' 
                                ps                      = splitPCs t0 
                                (cs, pcs)               = unzip ps
                                pc                      = foldr (/\) ttPC pcs
                            in  (T.concat cs, pc)
          --isCPPDirective = T.isPrefixOf (T.pack "#")