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
import qualified Data.MultiMap           as MM
import SPL
import PresenceCondition 

type VNode = Var CFGNode

dummyNode = CFGNode 0 T.empty (CFGDummy T.empty) [] []

mkV :: a -> (a, PCExpr) -> Var a
mkV dummy (v, pc) = 
    if   pc == ttPC
    then mkVar ttPC v
    else mkVars [(v, pc), (dummy, negPC pc)]

_mkVNode' = liftV5 CFGNode

lv2vl :: Show a => [Var a] -> Var [a]
lv2vl = foldr (|:|) (mkVar ttPC [])

getCodeFromField :: RecordField -> T.Text
getCodeFromField (FieldLabel l) = T.cons '@' l
getCodeFromField (FlipFields fs) = foldl T.append T.empty (map getCodeFromField fs)
getCodeFromField _ = T.empty

getCodeFromAttribute :: Attribute -> T.Text
getCodeFromAttribute (Label (RecordLabel fs)) =
     foldl T.append T.empty (map getCodeFromField fs)

getCodeFromAttribute (Label (StrLabel s)) = s

getCodeFromAttribute _ = T.empty

processEdge :: (T.Text, Attributes) -> IO (Int, PCExpr)
processEdge (t, as) = do
    let nodeID = read (T.unpack t)
    let ls = foldl T.append T.empty (map getCodeFromAttribute as)
    let pc = parsePC (T.unpack ls)
    return $ (nodeID, pc)

appendEdges :: CFGNode -> [Int] -> [Int] -> CFGNode 
appendEdges n ps ss = CFGNode (nID n) (text n) (ast n) ps ss

getCntxtContents :: Data.GraphViz.Types.Graph.Context T.Text -> IO (Var CFGNode)
getCntxtContents (Cntxt n _ as ps ss) = do
    let xs'         = foldl T.append T.empty (map getCodeFromAttribute as)
    let ys          = filter (not . T.null) $ T.splitOn (T.pack "@") xs'
    let (nt, pc)    =  case ys of
                        []              -> (CFGFunc T.empty, ffPC)
                        (x : [])        -> parseContext x
                        (x : y : [])    -> let (n', pc') = parseContext x
                                           in (n', pc' /\ (parsePC $ T.unpack y))
    ps'             <- mapM (\p -> do e <- (processEdge p)
                                      return $ mkV 0 e) ps
    ss'             <- mapM (\s -> do e <- (processEdge s)
                                      return $ mkV 0 e) ss
    let n' = mkV dummyNode (CFGNode (read (T.unpack n)) xs' nt [] [], pc)
    return $ (liftV3 appendEdges) n' (lv2vl ps') (lv2vl ss')

inputGraph :: String -> IO (DotGraph T.Text)
inputGraph inputFileName = readDotFile inputFileName 

showEdge ((CFGNode i0 _ _ _ _, CFGNode i1 _ _ _ _), pc) =
    (show i0) ++ " -> " ++ (show i1) ++ " @ " ++ (show pc)

collectNodeEdges :: CFGNode -> [(Int, Int)]
collectNodeEdges n =
    (map (\p -> (p, nID n)) (_preds n)) ++
    (map (\s -> (nID n, s)) (_succs n))

collectAllEdges :: [CFGNode] -> [(Int, Int)]
collectAllEdges ns =
    foldr (++) [] (map collectNodeEdges ns)

adjustEdges :: [CFGNode] -> [CFGNode]
adjustEdges ns =
    let edges       = collectAllEdges ns
        getPreds n  = map fst (filter (\(_, s) -> s == nID n && s /= 0) edges)
        getSuccs n  = map snd (filter (\(p, _) -> p == nID n && p /= 0) edges)
    in  map (\n -> CFGNode (nID n) (text n) (ast n) (getPreds n) (getSuccs n)) ns

mkCFG :: [CFGNode] -> CFG
mkCFG  ns = CFG $ foldr (\n m -> MM.insert (nID n) n m) MM.empty ns
mkCFG' = liftV mkCFG

length' = liftV length

readGraph :: String -> IO (Var CFG)
readGraph fn = do
    g <- inputGraph fn
    let ds = decomposeList g
    vs  <- mapM getCntxtContents ds
    let vs' = lv2vl vs
    let vs'' = trace ("readGraph: length(vs'): " ++ (show (length' vs'))) $ (liftV adjustEdges) vs'
    return $ trace ("readGraph: length(vs''): " ++ (show (length' vs''))) $ mkCFG' vs''

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
