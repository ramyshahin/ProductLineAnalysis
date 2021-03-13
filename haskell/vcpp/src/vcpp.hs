-- vcpp.hs
-- Variability-aware C PreProcessor
-- Ramy Shahin
-- Feb. 19th 2017
{-# LANGUAGE BangPatterns #-}

module VCPP where
import Lexer
import SPL
import PropBDD
import Data.Stack
import Debug.Trace
import Text.Parsec
--import Data.String.Utils(strip, splitWs, replace)

ttPC = tt
ffPC = ff

type CPPEnv = Stack Prop

mkCPPEnv = stackNew

parsePCAtom :: [CToken] -> (Prop, [CToken])
parsePCAtom [] = (ff, [])
parsePCAtom ((x,p) : xs) = 
    case x of
        TID id          -> if id == "defined" then parsePCAtom xs else (mkBDDVar id, xs)
        TParen ys       -> let (p,_) = parsePCAtom ys in (p, xs)
        TBang           -> let (p, cs) = parsePCAtom xs in (notBDD p, cs)
        TIntegerLit i   -> if i == 0 then (ff, xs) else (tt, xs)
        _               -> trace "*****invalid PC Atom" (ff,[])

parsePC :: Prop -> [CToken] -> Prop
parsePC cntxt [] = cntxt
parsePC cntxt xs@((t,p) : ts) = 
    case t of 
        TAmprAmpr -> andBDD cntxt (parsePC tt ts)
        TBarBar   -> orBDD  cntxt (parsePC tt ts) 
        _ -> let (a, cs) = parsePCAtom xs 
             in  parsePC a cs

processPPCommand :: CPPEnv -> [CToken] -> CPPEnv
processPPCommand env [] = trace "empty CPP command" env
processPPCommand env ((t,p) : ts) = 
    case t of -- TODO: process various CPP commands
        TID "include" -> env
        TID "define" -> env
        TIf          -> let pc = parsePC tt ts
                        in  stackPush env pc
        TID "ifdef"  -> let pc = parsePC tt ts
                        in  stackPush env pc
        TID "ifndef" -> let pc = Neg $ parsePC tt ts
                        in  stackPush env pc
        TElse        -> case stackPop env of
                            Nothing    -> trace "#else unmatching!" mkCPPEnv
                            Just (e,p) -> stackPush e (Neg p)
        TID "endif"  -> case stackPop env of
                            Nothing    -> trace "#endif unmatching!" mkCPPEnv
                            Just (e,_) -> e 
        _ -> trace ("#" ++ (show t) ++ " unsupported!") mkCPPEnv

liftToken :: CToken -> PresenceCondition -> Var CToken
liftToken t pc = 
    if pc == tt then 
        t ^| ttPC
    else let !pc' = notBDD pc
         in  mkVars [(t,pc), ((TNil, snd t), pc')]

vcpp :: CPPEnv -> [CToken] -> Var [CToken]
vcpp _ [] = [] ^| ttPC
vcpp env (t : ts) = 
    case fst t of
        TSharp      -> let  line = sourceLine $ snd t
                            s    = span (\(t,p) -> sourceLine p == line) ts
                            ppCommand = fst s
                            rest = snd s
                            env' = processPPCommand env ppCommand
                       in vcpp env' rest
        TParen xs   -> vcpp env $ ((TLParen, snd t) : xs) ++ ((TRParen, snd t) : ts) 
        TBracket xs -> vcpp env $ ((TLBracket, snd t) : xs) ++ ((TRBracket, snd t) : ts)
        TBrace xs   -> vcpp env $ ((TLBrace, snd t) : xs) ++ ((TRBrace, snd t) : ts)
        _           -> let pc' = stackPeek env 
                           pc  = case pc' of 
                                    Nothing -> tt 
                                    Just p  -> p
                       in (liftToken t pc) |:| vcpp env ts

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