-- vcpp.hs
-- Variability-aware C PreProcessor
-- Ramy Shahin
-- Feb. 19th 2017

module VCPP where
import Lexer
import SPL
import PropBDD
import Data.Stack
import Debug.Trace
import Text.Parsec
--import Data.String.Utils(strip, splitWs, replace)

type CPPEnv = Stack Prop

mkCPPEnv = stackNew

parsePC :: Prop -> [CToken] -> Prop
parsePC cntxt [] = cntxt
parsePC cntxt ((t,p) : ts) = 
    case t of 
        TID id -> if cntxt == tt then parsePC (Atom id) ts else trace "Invalid PC" cntxt
        _ -> trace "Invalid PC" cntxt

processPPCommand :: CPPEnv -> [CToken] -> CPPEnv
processPPCommand env [] = trace "empty CPP command" env
processPPCommand env ((t,p) : ts) = 
    case t of -- TODO: process various CPP commands
        TID "include" -> env
        TID "define" -> env
        TIf          -> trace ("#if " ++ show ts) env
        TID "ifdef"  -> let pc = parsePC tt ts
                        in  trace ("New PC " ++ show pc) (stackPush env pc)
        TID "ifndef" -> let pc = Neg $ parsePC tt ts
                        in  trace ("New PC " ++ show pc) (stackPush env pc)
        TElse        -> case stackPop env of
                            Nothing    -> trace "#else unmatching!" mkCPPEnv
                            Just (e,p) -> stackPush e (Neg p)
        TID "endif"  -> case stackPop env of
                            Nothing    -> trace "#endif unmatching!" mkCPPEnv
                            Just (e,_) -> e 
        _ -> trace ("#" ++ (show t) ++ " unsupported!") mkCPPEnv

vcpp :: CPPEnv -> [CToken] -> [Var CToken]
vcpp _ [] = []
vcpp env (t : ts) = 
    case fst t of
        TSharp -> let line = sourceLine $ snd t
                      s    = span (\(t,p) -> sourceLine p == line) ts
                      ppCommand = fst s
                      rest = snd s
                      env' = processPPCommand env ppCommand
                  in trace "# here" (vcpp env' rest)
        _      -> let pc' = stackPeek env 
                      pc  = case pc' of 
                                Nothing -> tt 
                                Just p  -> p
                  in (mkVar t pc) : vcpp env ts

{-
envToProp :: Stack Prop -> Prop
envToProp env =
    let p = stackPop env
    in  case p of
            Nothing -> tt 
            Just (env', t) -> conj[t, envToProp env'] -- TODO: make this tail-recursive
 
emptyLine = mkVar "" tt

-- spliceLines
--    remove all "\\\n" sequences
spliceLines :: String -> String
spliceLines = replace "\\\n" ""

-- lineByLine: 
--      [String] : a list of lines in a compilation unit
--      CPPEnv   : preprocessor environment
--      returns IO (Var [String]) : IO becuase #includes result in file I/O
lineByLine :: [String] -> CPPEnv -> IO (Var [String])
lineByLine [] _ = return (mkVarT [])
lineByLine (x:xs) e@(CPPEnv feats env) = 
    let x' = strip x
        tokens = splitWs x'
        token0 = tokens!!0
        token1 = tokens!!1
        envProp = envToProp env
    in  if (null tokens) || (head token0) /= '#' 
        then do
            rest <- lineByLine xs e
            return ((mkVars [(x,envProp), ("", neg envProp)]) |:| rest)
        -- #ifdef
        else if token0 == "#ifdef"
        then let (feats', p) = queryOrUpdate feats token1
                 env' = stackPush env p 
             in  do
                 rest <- lineByLine xs (CPPEnv feats' env')
                 return (emptyLine |:| rest)
        -- #else
        else if token0 == "#else"
        then let e' = stackPop env
             in  case e' of
                    Nothing        -> error ("error: " ++ x) 
                    Just (env', t) -> let env'' = stackPush env' (neg t)
                                      in do
                                          rest <- lineByLine xs (CPPEnv feats env'')
                                          return (emptyLine |:| rest)
        -- #endif
        else if token0 == "#endif"
        then let e' = stackPop env
             in  case e' of
                    Just (env', _) -> do
                                        rest <- lineByLine xs (CPPEnv feats env')
                                        return (emptyLine |:| rest)
                    Nothing        -> error ("error: " ++ x)
        -- #include
        else if token0 == "#include"
             -- #include "..."
             then if token1!!0 == '\"' 
                  then do
                        let fname = filter (\c -> c /= '\"') token1
                        incFile <- readFile fname
                        rest <- lineByLine ((lines incFile) ++ xs) e
                        return (emptyLine |:| rest) 
            else
                error ("error: " ++ x)
        else error ("error: " ++ x)

unlines' = pure unlines

vcpp :: String -> IO (Var String)
vcpp input = do
    ls <- lineByLine (lines (spliceLines input)) mkCPPEnv
    return (unlines' <*> ls) 
-}