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
                               "signed", "sizeof", "static", "struct", 
                               "switch", "typedef", "union", "unsigned", 
                               "void", "volatile", "while",
                               "#define", "#include", "#ifdef", "#else", "#endif"],
    Token.reservedOpNames   = ["++", "--", "&", "*", "+", "-", "~",
                               "!", "/", "%", "<<", ">>", "<", "<=", ">", ">=",
                               "==", "!=", "^", "|", "&&", "||", "?", ":", "=",
                               "*=", "/=", "%=", "+=", "-=", "<<=", ">>=", 
                               "&=", "^=", "|=", ",", ";", ".", "->"],
    Token.caseSensitive     = True
}

data CToken =
    TID          String
  | TOperator    String
  | TReservedOp  String
  | TCharLit     Char
  | TStringLit   String
  | TIntegerLit  Integer
  | TFloatLit    Double
  | TPlusPlus
  | TMinusMinus
  | TAmpr
  | TTimes
  | TPlus
  | TMinus 
  | TTilde
  | TBang
  | TDiv
  | TPercent
  | TLTLT
  | TGTGT
  | TLT
  | TLTEQ
  | TGT
  | TGTEQ
  | TEQEQ
  | TBangEQ
  | THat
  | TBar
  | TAmprAmpr
  | TBarBar
  | TQuest
  | TColon
  | TEQ
  | TTimesEQ
  | TDivEQ 
  | TPercentEQ
  | TPlusEQ 
  | TMinusEQ
  | TLTLTEQ
  | TGTGTEQ 
  | TAmprEQ 
  | THatEQ
  | TBarEQ
  | TComma
  | TSemi
  | TDot
  | TArrow
  | TAuto
  | TBreak
  | TCase
  | TChar
  | TConst 
  | TContinue
  | TDefault
  | TDo
  | TDouble
  | TElse
  | TEnum
  | TExtern
  | TFloat
  | TFor
  | TGoto
  | TIf
  | TInt
  | TLong
  | TRegister
  | TReturn
  | TShort
  | TSigned
  | TSizeof
  | TStatic
  | TStruct 
  | TSwitch 
  | TTypedef 
  | TUnion 
  | TUnsigned 
  | TVoid 
  | TVolatile
  | TWhile
  | TCPPDefine
  | TCPPInclude
  | TCPPIfdef
  | TCPPElse
  | TCPPEndIf
  deriving Show

cLexer = Token.makeTokenParser cLangDef

pId             = Token.identifier      cLexer
pReserved       = Token.reserved        cLexer
pOperator       = Token.operator        cLexer
pReservedOp     = Token.reservedOp      cLexer
pCharLiteral    = Token.charLiteral     cLexer 
pStringLiteral  = Token.stringLiteral   cLexer
pIntegerLit     = Token.integer         cLexer
pNatFloatLit    = Token.naturalOrFloat  cLexer
pWhiteSpace     = Token.whiteSpace      cLexer

pPlusPlus   = pReservedOp "++"
pMinusMinus = pReservedOp "--"
pAmpr       = pReservedOp "&"
pTimes      = pReservedOp "*"
pPlus       = pReservedOp "+"
pMinus      = pReservedOp "-" 
pTilde      = pReservedOp "~"
pBang       = pReservedOp "!"
pDiv        = pReservedOp "/"
pPercent    = pReservedOp "%"
pTLTLT      = pReservedOp "<<"
pGTGT       = pReservedOp ">>"
pLT         = pReservedOp "<"
pLTEQ       = pReservedOp "<="
pGT         = pReservedOp ">"
pGTEQ       = pReservedOp ">="
pEQEQ       = pReservedOp "=="
pBangEQ     = pReservedOp "!="
pHat        = pReservedOp "^"
pBar        = pReservedOp "|"
pAmprAmpr   = pReservedOp "&&"
pBarBar     = pReservedOp "||"
pQuest      = pReservedOp "?"
pColon      = pReservedOp ":"
pEQ         = pReservedOp "="
pTimesEQ    = pReservedOp "*="
pDivEQ      = pReservedOp "/="
pPercentEQ  = pReservedOp "%="
pPlusEQ     = pReservedOp "+="
pMinusEQ    = pReservedOp "-="
pLTLTEQ     = pReservedOp "<<="
pGTGTEQ     = pReservedOp ">>="
pAmprEQ     = pReservedOp "&="
pHatEQ      = pReservedOp "^="
pBarEQ      = pReservedOp "|="
pComma      = pReservedOp ","
pSemi       = pReservedOp ";"
pDot        = pReservedOp "."
pArrow      = pReservedOp "->"

pAuto           = pReserved "auto"
pBreak          = pReserved "break" 
pCase           = pReserved "case"
pChar           = pReserved "char"
pConst          = pReserved "const" 
pContinue       = pReserved "continue"
pDefault        = pReserved "default"
pDo             = pReserved "do"
pDouble         = pReserved "double"
pElse           = pReserved "else"
pEnum           = pReserved "enum"
pExtern         = pReserved "extern"
pFloat          = pReserved "float"
pFor            = pReserved "for"
pGoto           = pReserved "goto"
pIf             = pReserved "if"
pInt            = pReserved "int"
pLong           = pReserved "long"
pRegister       = pReserved "register"
pReturn         = pReserved "return"
pShort          = pReserved "short"
pSigned         = pReserved "signed"
pSizeof         = pReserved "sizeof"
pStatic         = pReserved "static"
pStruct         = pReserved "struct" 
pSwitch         = pReserved "switch"
pTypedef        = pReserved "typedef"
pUnion          = pReserved "union" 
pUnsigned       = pReserved "unsigned" 
pVoid           = pReserved "void"
pVolatile       = pReserved "volatile"
pWhile          = pReserved "while"
pCPPDefine      = pReserved "#define"
pCPPInclude     = pReserved "#include"
pCPPIfdef       = pReserved "#ifdef"
pCPPElse        = pReserved "#else"
pCPPEndIf       = pReserved "#endif"

genericP :: Parser CToken
genericP =  (pPlusPlus    >> return TPlusPlus)   
        <|> (pMinusMinus  >> return TMinusMinus)
        <|> (pAmpr        >> return TAmpr)
        <|> (pTimes       >> return TTimes)  
        <|> (pPlus       >> return TPlus)
        <|> (pMinus      >> return TMinus)
        <|> (pTilde      >> return TTilde)
        <|> (pBang       >> return TBang)
        <|> (pDiv        >> return TDiv)
        <|> (pPercent    >> return TPercent)
        <|> (pTLTLT      >> return TLTLT)
        <|> (pGTGT       >> return TGTGT)
        <|> (pLT         >> return TLT)
        <|> (pLTEQ       >> return TLTEQ)
        <|> (pGT         >> return TGT)
        <|> (pGTEQ       >> return TGTEQ)
        <|> (pEQEQ       >> return TEQEQ)
        <|> (pBangEQ     >> return TBangEQ)
        <|> (pHat        >> return THat)
        <|> (pBar        >> return TBar)
        <|> (pAmprAmpr   >> return TAmprAmpr)
        <|> (pBarBar     >> return TBarBar)
        <|> (pQuest      >> return TQuest)
        <|> (pColon      >> return TColon)
        <|> (pEQ         >> return TEQ)
        <|> (pTimesEQ    >> return TTimesEQ)
        <|> (pDivEQ      >> return TDivEQ)
        <|> (pPercentEQ  >> return TPercentEQ)
        <|> (pPlusEQ     >> return TPlusEQ)
        <|> (pMinusEQ    >> return TMinusEQ)
        <|> (pLTLTEQ     >> return TLTLTEQ)
        <|> (pGTGTEQ     >> return TGTGTEQ)
        <|> (pAmprEQ     >> return TAmprEQ)
        <|> (pHatEQ      >> return THatEQ)
        <|> (pBarEQ      >> return TBarEQ)
        <|> (pComma      >> return TComma)
        <|> (pSemi       >> return TSemi)
        <|> (pDot        >> return TDot)
        <|> (pArrow      >> return TArrow)
        <|> (pAuto >> return TAuto)
        <|> (pBreak >> return TBreak)
        <|> (pCase  >> return TCase)
        <|> (pChar  >> return TChar)
        <|> (pConst >> return TConst) 
        <|> (pContinue >> return TContinue)
        <|> (pDefault >> return TDefault)
        <|> (pDo >> return TDo)
        <|> (pDouble >> return TDouble)
        <|> (pElse >> return TElse)
        <|> (pEnum >> return TEnum)
        <|> (pExtern >> return TExtern)
        <|> (pFloat >> return TFloat)
        <|> (pFor >> return TFor)
        <|> (pGoto >> return TGoto)
        <|> (pIf >> return TIf)
        <|> (pInt >> return TInt)
        <|> (pLong >> return TLong)
        <|> (pRegister >> return TRegister)
        <|> (pReturn >> return TReturn)
        <|> (pShort >> return TShort)
        <|> (pSigned >> return TSigned)
        <|> (pSizeof >> return TSizeof)
        <|> (pStatic >> return TStatic)
        <|> (pStruct >> return TStruct) 
        <|> (pSwitch >> return TSwitch) 
        <|> (pTypedef >> return TTypedef) 
        <|> (pUnion >> return TUnion) 
        <|> (pUnsigned >> return TUnsigned) 
        <|> (pVoid >> return TVoid) 
        <|> (pVolatile >> return TVolatile)
        <|> (pWhile >> return TWhile)
        <|> (pCPPInclude >> return TCPPInclude)
        <|> (pCPPDefine  >> return TCPPDefine)
        <|> (pCPPIfdef   >> return TCPPIfdef)
        <|> (pCPPElse    >> return TCPPElse)
        <|> (pCPPEndIf   >> return TCPPEndIf)
        <|> (do id <- pId; return $ TID id) 
        <|> (do op <- pOperator; return $ TOperator op) 
        <|> (do c  <- pCharLiteral; return $ TCharLit c)
        <|> (do s  <- pStringLiteral; return $ TStringLit s)
        <|> (do v  <- pNatFloatLit; 
                return $ case v of 
                            Left  i -> TIntegerLit i
                            Right f -> TFloatLit f)
        <|> (do i  <- pIntegerLit; return $ TIntegerLit i)  

lexer = parse $ do { pWhiteSpace; ts <- many genericP; eof ; return ts }
