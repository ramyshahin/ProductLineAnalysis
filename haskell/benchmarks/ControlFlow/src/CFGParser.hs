module CFGParser where

import CFG
import Data.GraphViz.Types.Graph
import Data.GraphViz.Commands.IO
import Data.GraphViz.Parsing
import Data.GraphViz.Attributes.Complete
import Language.C.Parser
import Language.C.Data.InputStream
import Language.C.Data.Position
import Language.C.Syntax.AST
import Debug.Trace
import Control.Exception
import qualified Data.Text.Lazy     as T
import qualified Data.List          as L
import qualified Data.Maybe         as M
import qualified Data.HashTable.IO  as H
import SPL
import PresenceCondition 

type VNode = Var CFGNode

dummyNode = CFGNode 0 T.empty (CFGDummy T.empty) [] []

mkVNode :: CFGNode -> PCExpr -> VNode
mkVNode n pc = 
    mkVars [(n, pc), (dummyNode, negPC pc)]

_mkVNode :: Int -> T.Text -> NodeType -> [CFGNode] -> [CFGNode] -> CFGNode
_mkVNode i t nt ps ss = CFGNode i t nt ps ss

_mkVNode' = liftV5 _mkVNode

fixCompleteness :: Var a -> a -> Var a
fixCompleteness v x = SPL.union v (mkVar x (undefinedAt v))

lv2vl :: [Var a] -> Var [a]
lv2vl = foldr (|:|) (mkVarT [])

type HashTable k v = H.CuckooHashTable k v

type Text2Node      = HashTable T.Text CFGNode
type Text2Cntxt     = HashTable T.Text (Context T.Text)

getCodeFromField :: RecordField -> T.Text
getCodeFromField (FieldLabel l) = T.cons '@' l
getCodeFromField (FlipFields fs) = foldl T.append T.empty (map getCodeFromField fs)
getCodeFromField _ = T.empty

getCodeFromAttribute :: Attribute -> T.Text
getCodeFromAttribute (Label (RecordLabel fs)) =
     foldl T.append T.empty (map getCodeFromField fs)

getCodeFromAttribute (Label (StrLabel s)) = s

getCodeFromAttribute _ = T.empty

findNode :: Text2Node -> Text2Cntxt -> T.Text -> IO CFGNode
findNode txt2node txt2cntxt t = do
    n' <- H.lookup txt2node t 
    case n' of
        Nothing -> do cntxt' <- H.lookup txt2cntxt t
                      case cntxt' of
                          Nothing -> fail $ "Context not found for node " ++ (show t)
                          Just c  -> do (Var n) <- getCntxtContents txt2node txt2cntxt c;
                                        assert (length n == 1) $ return (fst $ head n)
        Just n  -> return n

processEdge :: Text2Node -> Text2Cntxt -> (T.Text, Attributes) -> IO VNode
processEdge txt2node txt2cntxt (t, as) = do
    let ls = foldl T.append T.empty (map getCodeFromAttribute as)
    let pc = parsePC (T.unpack ls)
    n <- findNode txt2node txt2cntxt t
    return $ mkVNode n pc

getCntxtContents :: Text2Node -> Text2Cntxt -> Context T.Text -> IO VNode
getCntxtContents txt2node txt2cntxt (Cntxt n _ as ps ss) = do
    let xs'         = foldl T.append T.empty (map getCodeFromAttribute as)
    let ys          = filter (not . T.null) $ T.splitOn (T.pack "@") xs'
    let (nt, pc)    = case ys of
                        []              -> (CFGFunc T.empty, ffPC)
                        (x : [])        -> parseContext x
                        (x : y : [])    -> let (n', pc') = parseContext x
                                           in (n', pc' /\ (parsePC $ T.unpack y))
    ps'             <- mapM (processEdge txt2node txt2cntxt) ps
    ss'             <- mapM (processEdge txt2node txt2cntxt) ss
    let n''         = _mkVNode' (mkVar (read (T.unpack n)) pc) (mkVarT xs') (mkVarT nt) (lv2vl ps') (lv2vl ss')
    let n'@(Var _n) = fixCompleteness n'' dummyNode
    assert (length _n == 1) H.insert txt2node n (fst $ head _n)
    return n'

createTxt2Cntxt :: [Context T.Text] -> IO Text2Cntxt
createTxt2Cntxt cs = do
    txt2cntxt <- H.new
    mapM (\c@(Cntxt n _ _ _ _) -> H.insert txt2cntxt n c) cs 
    return txt2cntxt 

inputGraph :: String -> IO (DotGraph T.Text)
inputGraph inputFileName = readDotFile inputFileName 

readGraph :: String -> IO (Var [CFGNode])
readGraph fn = do
    g <- inputGraph fn
    let ds = decomposeList g
    txt2node <- H.new
    txt2cntxt <- createTxt2Cntxt ds
    vs <- mapM (getCntxtContents txt2node txt2cntxt) ds
    let vs' = lv2vl vs
    return vs'

-- parsing node expression/statement
parseDeclaration :: Int -> String -> Bool -> NodeType
parseDeclaration lineNum s tryAppend =
    let input = inputStreamFromString s 
        p     = execParser_ extDeclP input (position 0 "" lineNum 0 Nothing)
    in  case p of
            Left e -> if tryAppend 
                      then appendDummy lineNum s
                      else trace ("Dummy statement node: " ++ s) $ CFGDummy (T.pack s)
            Right e -> CFGVarDecl e

appendDummy :: Int -> String -> NodeType
appendDummy lineNum s =
    let s' = s ++ "x += 0;"
    in  parseStatement lineNum s' False

parseStatement :: Int -> String -> Bool -> NodeType
parseStatement lineNum s tryAppend = 
    let input = inputStreamFromString s 
        p     = execParser_ statementP input (position 0 "" lineNum 0 Nothing)
    in  case p of
            Left e -> parseDeclaration lineNum s tryAppend
            Right e -> CFGStat e

parseExpr :: Int -> String -> NodeType 
parseExpr lineNum s = 
    let input = inputStreamFromString s 
        p     = execParser_ expressionP input (position 0 "" lineNum 0 Nothing)
    in  case p of
            Left e -> trace ("Dummy expression node: " ++ s) $ CFGDummy (T.pack s)
            Right e -> CFGExpr e

parseContext :: T.Text -> (NodeType, PCExpr)
parseContext t = 
    let (nodeType, rest) = T.break (== ' ') t
        (lineNo, rest')  = if (T.null rest) then (T.pack "0", T.empty) else T.break (== ':') (T.tail rest)
        lineNum          = read (T.unpack lineNo)
        nodeText'        = if (T.null rest') then T.empty else T.tail rest'
        (nodeText,pc)    = fixCFragment nodeText'
        ast              = case T.unpack nodeType of
                                "Stmt"      -> parseStatement lineNum (T.unpack nodeText) True
                                "Expr"      -> parseExpr lineNum (T.unpack nodeText)
                                "Decl"      -> CFGDecl $ T.strip nodeText
                                "Function"  -> CFGFunc $ T.strip nodeText
                                _           -> CFGDummy nodeText
    in  (ast, pc)
    where fixCFragment s =  let t0                      = fixQuotes s
                                t1                      = T.splitOn (T.pack "\\l") t0
                                (pcFields, codeFields)  = L.partition (T.isPrefixOf (T.pack "#if")) t1 
                                pcs                     = map (parsePC . T.unpack . 
                                                                (\p -> M.maybe p id 
                                                                       (T.stripPrefix (T.pack "#if definedEx") p)))
                                                              pcFields
                            in  (T.concat (filter (not . isCPPDirective) codeFields),
                                 foldr (/\) ttPC pcs)
          fixQuotes      = (T.replace (T.pack "\\\"") (T.pack "\""))
                         . (T.replace (T.pack "\\\'") (T.pack "\'"))
          isCPPDirective = T.isPrefixOf (T.pack "#")