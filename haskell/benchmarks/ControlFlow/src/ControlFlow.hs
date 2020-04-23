module ControlFlow where

import Data.GraphViz.Types.Graph
import Data.GraphViz.Commands.IO
import Data.GraphViz.Parsing
import Data.GraphViz.Attributes.Complete
import Language.C.Parser
import Language.C.Data.InputStream
import Language.C.Data.Position
import Language.C.Syntax.AST
import Debug.Trace
import qualified Data.Text.Lazy as T
import qualified Data.List      as L
import qualified Data.Maybe     as M
import PresenceCondition 

data CFGNodeType =
    CFGExpr     CExpr
  | CFGStat     CStat
  | CFGVarDecl  CExtDecl
  | CFGDecl     T.Text
  | CFGFunc     T.Text
  | CFGDummy    T.Text
  deriving Show

data CFGNode = CFGNode T.Text
    deriving (Eq, Ord, Show)

instance ParseDot CFGNode where
    parse = do t <- (parse :: Parse T.Text); return $ CFGNode t
    parseUnqt = parse
    parseUnqtList = many parseUnqt 
    parseList = many parse

inputFileName = "/mnt/f/code/busybox-1.18.5/coreutils/head.cfg.dot"

inputGraph :: IO (DotGraph CFGNode)
inputGraph = readDotFile inputFileName 

getCodeFromField :: RecordField -> T.Text
getCodeFromField (FieldLabel l) = T.cons '@' l
getCodeFromField (FlipFields fs) = foldl T.append T.empty (map getCodeFromField fs)
getCodeFromField _ = T.empty

getCodeFromAttribute :: Attribute -> T.Text
getCodeFromAttribute (Label (RecordLabel fs)) =
     foldl T.append T.empty (map getCodeFromField fs)

getCodeFromAttribute _ = T.empty

getCntxtContents :: Context CFGNode -> (CFGNodeType, PCExpr)
getCntxtContents (Cntxt _ _ as _ _) =
    let xs' = foldl T.append T.empty (map getCodeFromAttribute as)
        --xs  = T.drop 1 xs' -- strip leading and trailing quotes
        ys = filter (not . T.null) $ T.splitOn (T.pack "@") xs'
    in  case ys of 
            []              -> (CFGFunc T.empty, ffPC)
            (x : [])        -> parseContext x
            (x : y : [])    -> let (n', pc') = parseContext x
                               in (n', pc' /\ (parsePC $ T.unpack y))

debugGraph :: DotGraph CFGNode -> IO ()
debugGraph g = do
    let ds = decomposeList g
    mapM_ (\c -> putStrLn $ "Node: " ++ show (getCntxtContents c)) ds

-- parsing node expression/statement
parseDeclaration :: String -> Bool -> CFGNodeType
parseDeclaration s tryAppend =
    let input = inputStreamFromString s 
        p     = execParser_ extDeclP input nopos
    in  case p of
            Left e -> if tryAppend 
                      then appendDummy s
                      else trace ("Dummy statement node: " ++ s) $ CFGDummy (T.pack s)
            Right e -> CFGVarDecl e

appendDummy :: String -> CFGNodeType
appendDummy s =
    let s' = s ++ "x += 0;"
    in  parseStatement s' False

parseStatement :: String -> Bool -> CFGNodeType
parseStatement s tryAppend = 
    let input = inputStreamFromString s 
        p     = execParser_ statementP input nopos
    in  case p of
            Left e -> parseDeclaration s tryAppend
            Right e -> CFGStat e

parseExpr :: String -> CFGNodeType 
parseExpr s = 
    let input = inputStreamFromString s 
        p     = execParser_ expressionP input nopos
    in  case p of
            Left e -> trace ("Dummy expression node: " ++ s) $ CFGDummy (T.pack s)
            Right e -> CFGExpr e

parseContext :: T.Text -> (CFGNodeType, PCExpr)
parseContext t = 
    let (nodeType, rest) = T.break (== ' ') t
        (lineNo, rest')  = T.break (== ':') (T.tail rest)
        nodeText'        = T.tail rest'
        (nodeText,pc)    = fixCFragment nodeText'
        ast              = case T.unpack nodeType of
                                "Stmt"      -> parseStatement (T.unpack nodeText) True
                                "Expr"      -> parseExpr (T.unpack nodeText)
                                "Decl"      -> CFGDecl $ T.strip nodeText
                                "Function"  -> CFGFunc $ T.strip nodeText
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