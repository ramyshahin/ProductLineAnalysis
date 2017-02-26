-- vcpp.hs
-- Variability-aware C PreProcessor
-- Ramy Shahin
-- Feb. 19th 2017
module VCPP where
import SPL
import Prop
import Data.Stack
import Data.String.Utils(strip, splitWs)

data CPPEnv = CPPEnv Universe (Stack Prop)

mkCPPEnv = CPPEnv (mkUniverse []) stackNew

envToProp :: Stack Prop -> Prop
envToProp env =
    let p = stackPop env
    in  case p of
            Nothing -> T 
            Just (env', t) -> conj[t, envToProp env'] -- TODO: make this tail-recursive
 
lineByLine :: [String] -> CPPEnv -> Var [String]
lineByLine [] _ = mkVarT []
lineByLine (x:xs) (CPPEnv feats env) = 
    let x' = strip x
        tokens = splitWs x'
        token0 = tokens!!0
        token1 = tokens!!1
        envProp = envToProp env
    in  if (null tokens) || (head token0) /= '#' 
        then (mkVar x envProp) |:| (lineByLine xs (CPPEnv feats env))
        -- #ifdef
        else if token0 == "#ifdef"
        then let (feats', i) = queryOrUpdate feats token1
                 p = Atom feats' i
                 env' = stackPush env p 
             in  (mkVar "\n" envProp) |:| (lineByLine xs (CPPEnv feats' env'))
        -- #else
        else if token0 == "#else"
        then let e' = stackPop env
             in  case e' of
                    Nothing        -> error ("error: " ++ x) 
                    Just (env', t) -> let env'' = stackPush env' (neg t)
                                      in  (mkVar "\n" envProp) |:| (lineByLine xs (CPPEnv feats env''))
        -- #endif
        else if token0 == "#endif"
        then let e' = stackPop env
             in  case e' of
                    Just (env', _) -> (mkVar "\n" envProp) |:| (lineByLine xs (CPPEnv feats env'))
                    Nothing        -> error ("error: " ++ x)
        else error ("error: " ++ x)

unlines' = pure unlines

vcpp :: String -> Var String
vcpp input =
    unlines' <*> (lineByLine (lines input) mkCPPEnv)

