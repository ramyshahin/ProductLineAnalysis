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
import PresenceCondition

data CFGNode = CFGNode T.Text
    deriving (Eq, Ord, Show)

instance ParseDot CFGNode where
    parse = do t <- (parse :: Parse T.Text); return $ CFGNode t
    parseUnqt = parse
    parseUnqtList = many parseUnqt 
    parseList = many parse

inputFileName = "/mnt/f/code/busybox-1.18.5/coreutils/usleep.cfg.dot"

inputGraph :: IO (DotGraph CFGNode)
inputGraph = readDotFile inputFileName

parseStatement s = 
    let input = inputStreamFromString s 
    in  execParser_ statementP input nopos

parseExpr s = 
    let input = inputStreamFromString s 
    in  execParser_ expressionP input nopos 

getCodeFromField :: RecordField -> T.Text
getCodeFromField (FieldLabel l) = T.cons '@' l
getCodeFromField (FlipFields fs) = foldl T.append T.empty (map getCodeFromField fs)
getCodeFromField _ = T.empty

getCodeFromAttribute :: Attribute -> T.Text
getCodeFromAttribute (Label (RecordLabel fs)) =
     foldl T.append T.empty (map getCodeFromField fs)

getCodeFromAttribute _ = T.empty

getCntxtContents :: Context CFGNode -> (T.Text, PCExpr)
getCntxtContents (Cntxt _ _ as _ _) =
    let xs' = foldl T.append T.empty (map getCodeFromAttribute as)
        --xs  = T.drop 1 xs' -- strip leading and trailing quotes
        ys = filter (not . T.null) $ T.splitOn (T.pack "@") xs'
    in  case ys of 
            [] -> (T.empty, FF)
            (x : []) -> (x, FF)
            (x : y : []) -> (x, parsePC $ T.unpack y)

debugGraph :: DotGraph CFGNode -> IO ()
debugGraph g = do
    let ds = decomposeList g
    mapM_ (\c -> putStrLn $ "Node: " ++ show (getCntxtContents c)) ds
