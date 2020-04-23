module PresenceCondition where

import PropBDD
import System.IO 
import Control.Monad
import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Expr 
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token 

type PCExpr = Prop

(/\) = andBDD
(\/) = orBDD
negPC = notBDD
ttPC = tt
ffPC = ff

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
bOperators = [ [ Prefix (reservedOp "!"   >> return neg) ],
               [ Infix  (reservedOp "/\\" >> return (/\)) AssocLeft],
               [ Infix  (reservedOp "\\/" >> return (\/))  AssocLeft],
               [ Infix  (reservedOp "&&" >> return (/\)) AssocLeft],
               [ Infix  (reservedOp "||" >> return (\/))  AssocLeft],
               [ Infix  (reservedOp "&" >> return (/\)) AssocLeft],
               [ Infix  (reservedOp "|" >> return (\/))  AssocLeft]
             ]

bTerm =  parens pcExpr 
     <|> (reserved "tt" >> return TT)
     <|> (reserved "True" >> return TT)
     <|> (reserved "ff" >> return FF)
     <|> (reserved "False" >> return FF)
     <|> (reserved "definedEx" >> parens (liftM mkBDDVar identifier))
     <|> (reserved "def" >> parens (liftM mkBDDVar identifier))
     <|> (integer >>= \i -> if i == 0 then return FF else return TT)
     <|> liftM mkBDDVar identifier

pcExpr :: Parser PCExpr 
pcExpr = buildExpressionParser bOperators bTerm 

parsePC :: String -> PCExpr
parsePC str =
  case parse pcExpr "" str of
    Left e  -> error $ show e
    Right r -> r
