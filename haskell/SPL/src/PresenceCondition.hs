module PresenceCondition(
    PresenceCondition,
    (/\),
    (\/),
    negPC,
    contains,
    intersect,
    empty,
    allConfigs,
    noConfigs
) where

import PropBDD
import System.IO 
import Control.Monad
import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Expr 
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token 
import qualified Data.Set as S

type PresenceCondition  = Prop
type Context            = PresenceCondition
type PCExpr             = Prop

{-# INLINE (/\) #-}
(/\) = andBDD

{-# INLINE (\/) #-}
(\/) = orBDD

{-# INLINE negPC #-}
negPC = notBDD

{-# INLINE empty #-}
empty = unsat

{-# INLINE contains #-}
contains = implies

{-# INLINE intersect #-}
intersect = disj

languageDef = 
    emptyDef {
        Token.commentStart = "/*",
        Token.commentEnd   = "*/", 
        Token.commentLine  = "//",
        Token.identStart   = letter <|> char '_',
        Token.identLetter  = alphaNum <|> char '_',
        Token.reservedNames = ["tt", "ff", "True", "False", "def", "definedEx"],
        Token.reservedOpNames = ["/\\", "&", "&&", "\\/", "|", "||", "!"]
    }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
--semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace

--pcParser :: Parser PCExpr
--pcParser = whiteSpace >> pc 

--pc :: Parser PCExpr
--pc = parens pc

-- parsing subexpressions
bOperators = [ [ Prefix (reservedOp "!"   >> return (negPC)) ],
               [ Infix  (reservedOp "/\\" >> return (/\)) AssocLeft],
               [ Infix  (reservedOp "\\/" >> return (\/))  AssocLeft],
               [ Infix  (reservedOp "&&" >> return (/\)) AssocLeft],
               [ Infix  (reservedOp "||" >> return (\/))  AssocLeft],
               [ Infix  (reservedOp "&" >> return (/\)) AssocLeft],
               [ Infix  (reservedOp "|" >> return (\/))  AssocLeft]
             ]

bTerm =  parens pcExpr 
     <|> (reserved "tt" >> return tt)
     <|> (reserved "True" >> return tt)
     <|> (reserved "ff" >> return ff)
     <|> (reserved "False" >> return ff)
     <|> (reserved "definedEx" >> parens (liftM mkBDDVar identifier))
     <|> (reserved "def" >> parens (liftM mkBDDVar identifier))
     <|> (integer >>= \i -> if i == 0 then return ff else return tt)
     <|> liftM mkBDDVar identifier

pcExpr :: Parser PCExpr
pcExpr = buildExpressionParser bOperators bTerm 

parsePC :: String -> PCExpr
parsePC str =
  case parse pcExpr "" str of
    Left e  -> error $ show e
    Right r -> r

mkFeature :: String -> PCExpr
mkFeature f = mkBDDVar f

{-
getPCFeatures' :: Prop' -> S.Set String
getPCFeatures' pc = 
  case pc of
    TT        -> S.empty
    FF        -> S.empty
    Atom s    -> S.singleton s 
    Neg p     -> getPCFeatures' p
    And l r   -> S.union (getPCFeatures' l) (getPCFeatures' r)
    Or  l r   -> S.union (getPCFeatures' l) (getPCFeatures' r) 

getPCFeatures :: PCExpr -> [String]
getPCFeatures = S.toList . getPCFeatures' . p
-}

getAllConfigs :: [String] -> [PCExpr]
getAllConfigs [] = []
getAllConfigs (f : fs) = 
    let p   = mkFeature f
        n   = negPC p
        fs' = getAllConfigs fs
    in  if   null fs 
        then [p, n]
        else (map (/\ p) fs') ++ (map (/\ n) fs')

{-
getAllConfigs :: [Prop] -> [Prop]
getAllConfigs [] = []
getAllConfigs (f:[]) = [f, (neg f)]
getAllConfigs (f:fs) = fPos ++ fNeg 
    where   rest = getAllConfigs fs
            fPos = map (\r -> conj[f,r]) rest
            fNeg = map (\r -> conj[(neg f), r]) rest
-}


--getValidConfigs :: [String] -> PCExpr -> [PCExpr]
--getValidConfigs univ featModel = filter (\c -> sat (conj[c,featModel])) cs 
--    where cs = getAllConfigs univ

allConfigs = tt
noConfigs = ff