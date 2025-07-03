module SuperCAST where 

import qualified Data.Text.IO       as TIO
import qualified Data.Text          as T
import qualified Data.List          as L
import           Data.Char
import           Debug.Trace
import           SPL
import           PresenceCondition

data ASTNode =
    ASTNode String [ASTNode]

showASTNode :: Int -> ASTNode -> String
showASTNode i (ASTNode s cs) =
    let ind = replicate (i*4) ' '
        p = ind ++ "{" ++ s ++ (if length cs > 0 then "\n" else "")
        p' = if length cs > 0 then (foldl (++) p (map (showASTNode (i+1)) cs)) ++ ind else p
    in p' ++ "}\n"

instance Show ASTNode where
    show = showASTNode 0

isPC :: String -> Bool
isPC s = L.isPrefixOf "(defined" s || L.isPrefixOf "!(defined" s

isID :: String -> Bool
isID s = last s == '('

isClosing :: String -> Bool
isClosing s = s == ")"

isClosingWithComma :: String -> Bool
isClosingWithComma s = s == "),"

endsWithComma :: String -> Bool
endsWithComma s = last s == ','

isNullaryID :: String -> Bool
isNullaryID s = last s == ')' && length s > 1

processLines :: [String] -> ([String], [ASTNode])
processLines ls =
    case ls of
        [] -> ([],[])
        s : ls' ->
            if isPC s then 
                let (rs, ns) = processLines ls'
                in (rs, (ASTNode ((show . parsePC) s) []) : ns)
            else if isClosingWithComma s then (ls', [])
            else if endsWithComma s then
                let s' = take ((length s) - 1) s
                    (_, ns) = processLines [s']
                    (rs, ns') = processLines ls'
                in  (rs, ns ++ ns')
            else if isNullaryID s then 
                let id = take ((length s) - 2) s
                    (rest, ns) = processLines ls'
                in (rest, (ASTNode id []) : ns)
            else if isID s then 
                let id = take ((length s) - 1) s
                    (rest, children) = processLines ls'
                    (rs, ns) = processLines rest
                in (rs, (ASTNode id children) : ns)
            else if isClosing s then (ls', [])
            else if head s == '\"' && last s == '\"' || all isNumber s then 
                let (rs, ns) = processLines ls'
                in (rs, (ASTNode s []) : ns)
            else trace ("not handled: " ++ s) (processLines ls')

trim :: String -> String
trim s = L.dropWhileEnd isSpace $ L.dropWhile isSpace s 

parseASTFile :: String -> IO ASTNode
parseASTFile filename = do
    fileTxt <- TIO.readFile filename
    let lines = (L.nub . T.lines) fileTxt
    let (rest, nodes) = processLines (map (trim . T.unpack) lines)
    putStrLn $ "Left overs: " ++ show rest
    return (head nodes)

{----------------
   Tokenization
-----------------}
type Token = String
type Tokens = [Token]

processTokenLine :: String -> PresenceCondition -> ([(Token, PresenceCondition)], PresenceCondition)
processTokenLine l context =
    if L.isPrefixOf "#if" l then
        let pc = parsePC $ drop 4 l
        in  ([], context /\ pc)
    else if L.isPrefixOf "#endif" l then
        ([], noConfigs)
    else
        let tokens = words l
        in (zip tokens (repeat context), context)

processTokenLines :: [String] -> PresenceCondition -> ([(Token, PresenceCondition)], PresenceCondition, [String])
processTokenLines ls context =
    case ls of
        [] -> ([], allConfigs, [])
        l : ls' -> 
            let (ts, cntxt') = processTokenLine l context in
            if cntxt' == noConfigs then
                (ts, noConfigs, ls')
            else if context /= cntxt' then
                let (ts', pc, rest) = processTokenLines ls' cntxt' 
                    (ts'', pc', rest') = processTokenLines rest context
                in  (ts' ++ ts'', context, rest')
            else 
                let (ts', pc, rest) = processTokenLines ls' context 
                in  (ts ++ ts', pc, rest)

parseTokensFile :: String -> IO [(Token, PresenceCondition)]
parseTokensFile filename = do
    fileTxt <- TIO.readFile filename
    let lines = map (trim . T.unpack) $ (L.nub . T.lines) fileTxt
    let (ts, _, _) = processTokenLines lines allConfigs 
    return ts