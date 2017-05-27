-- vcpp.hs
-- Variability-aware C PreProcessor
-- Ramy Shahin
-- Feb. 19th 2017
module VCPP where
import SPL
import PropBDD
import Data.Stack
import Data.String.Utils(strip, splitWs, replace)

data CPPEnv = CPPEnv Universe (Stack Prop)

mkCPPEnv = CPPEnv (mkUniverse []) stackNew

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
        then let (feats', i) = queryOrUpdate feats token1
                 p = Atom feats' i
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

