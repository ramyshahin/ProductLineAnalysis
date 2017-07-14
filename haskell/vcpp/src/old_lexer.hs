-- lexer.hs
--  A C language lexer
-- Ramy Shahin
-- March 2nd 2017

module Lexer where 

import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language 
import qualified Text.ParserCombinators.Parsec.Token as Token

cLangDef = emptyDef {
    Token.commentStart      = "/*",
    Token.commentEnd        = "*/",
    Token.commentLine       = "//",
    Token.nestedComments    = True,
    Token.identStart        = letter <|> char '_',
    Token.identLetter       = alphaNum <|> char '_',
    Token.reservedNames     = ["auto", "break", "case", "char", "const", 
                               "continue", "default", "do", "double", "else",
                               "enum", "extern", "float", "for", "goto", "if",
                               "int", "long", "register", "return", "short",
                               "signed", {-"sizeof",-} "static", "struct", 
                               "switch", "typedef", "union", "unsigned", 
                               "void", "volatile", "while"],
    Token.reservedOpNames   = ["++", "--", "sizeof", "&", "*", "+", "-", "~",
                               "!", "/", "%", "<<", ">>", "<", "<=", ">", ">=",
                               "==", "!=", "^", "|", "&&", "||", "?", ":", "=",
                               "*=", "/=", "%=", "+=", "-=", "<<=", ">>=", 
                               "&=", "^=", "|=", ","],
    Token.caseSensitive     = True
}

cLexer = Token.makeTokenParser cLangDef
