-----------------------------------------------------------------------------
-- |
-- Module      :  CppIfdef
-- Copyright   :  1999-2004 Malcolm Wallace
-- Licence     :  LGPL
-- 
-- Maintainer  :  Malcolm Wallace <Malcolm.Wallace@cs.york.ac.uk>
-- Stability   :  experimental
-- Portability :  All

-- Perform a cpp.first-pass, gathering \#define's and evaluating \#ifdef's.
-- and \#include's.
-----------------------------------------------------------------------------

module Language.Preprocessor.Cpphs.CppIfdef
  ( cppIfdef	-- :: FilePath -> [(String,String)] -> [String] -> Options
		--      -> String -> IO [(Posn,String)]
  ) where


import Language.Preprocessor.Cpphs.SymTab
import Text.ParserCombinators.HuttonMeijer
import Language.Preprocessor.Cpphs.Position  (Posn,newfile,newline,newlines
                                             ,cppline,cpp2hask,newpos)
import Language.Preprocessor.Cpphs.ReadFirst (readFirst)
import Language.Preprocessor.Cpphs.Tokenise  (linesCpp,reslash)
import Language.Preprocessor.Cpphs.Options   (BoolOptions(..))
import Language.Preprocessor.Cpphs.HashDefine(HashDefine(..),parseHashDefine
                                             ,expandMacro)
import Language.Preprocessor.Cpphs.MacroPass (preDefine,defineMacro)
import Data.Char      (isDigit)
import Numeric   (readHex,readOct,readDec)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.IO        (hPutStrLn,stderr)
import Control.Monad (when)


-- | Run a first pass of cpp, evaluating \#ifdef's and processing \#include's,
--   whilst taking account of \#define's and \#undef's as we encounter them.
cppIfdef :: FilePath		-- ^ File for error reports
	-> [(String,String)]	-- ^ Pre-defined symbols and their values
	-> [String]		-- ^ Search path for \#includes
	-> BoolOptions		-- ^ Options controlling output style
	-> String		-- ^ The input file content
	-> IO [(Posn,String)]	-- ^ The file after processing (in lines)
cppIfdef fp syms search options =
    cpp posn defs search options (Keep []) . (cppline posn:) . linesCpp
  where
    posn = newfile fp
    defs = preDefine options syms
-- Previous versions had a very simple symbol table  mapping strings
-- to strings.  Now the #ifdef pass uses a more elaborate table, in
-- particular to deal with parameterised macros in conditionals.


-- | Internal state for whether lines are being kept or dropped.
--   In @Drop n b ps@, @n@ is the depth of nesting, @b@ is whether
--   we have already succeeded in keeping some lines in a chain of
--   @elif@'s, and @ps@ is the stack of positions of open @#if@ contexts,
--   used for error messages in case EOF is reached too soon.
data KeepState = Keep [Posn] | Drop Int Bool [Posn]

-- | Return just the list of lines that the real cpp would decide to keep.
cpp :: Posn -> SymTab HashDefine -> [String] -> BoolOptions -> KeepState
       -> [String] -> IO [(Posn,String)]

cpp _ _ _ _ (Keep ps) [] | not (null ps) = do
    hPutStrLn stderr $ "Unmatched #if: positions of open context are:\n"++
                       unlines (map show ps)
    return []
cpp _ _ _ _ _ [] = return []

cpp p syms path options (Keep ps) (l@('#':x):xs) =
    let ws = words x
        cmd = if null ws then "" else head ws
        line = tail ws
        sym  = head (tail ws)
        rest = tail (tail ws)
        def = defineMacro options (sym++" "++ maybe "1" id (un rest))
        un v = if null v then Nothing else Just (unwords v)
        keepIf b = if b then Keep (p:ps) else Drop 1 False (p:ps)
        skipn syms' retain ud xs' =
            let n = 1 + length (filter (=='\n') l) in
            (if macros options && retain then emitOne  (p,reslash l)
                                         else emitMany (replicate n (p,""))) $
            cpp (newlines n p) syms' path options ud xs'
    in case cmd of
	"define" -> skipn (insertST def syms) True (Keep ps) xs
	"undef"  -> skipn (deleteST sym syms) True (Keep ps) xs
	"ifndef" -> skipn syms False (keepIf (not (definedST sym syms))) xs
	"ifdef"  -> skipn syms False (keepIf      (definedST sym syms)) xs
	"if"     -> skipn syms False
                               (keepIf (gatherDefined p syms (unwords line))) xs
	"else"   -> skipn syms False (Drop 1 False ps) xs
	"elif"   -> skipn syms False (Drop 1 True ps) xs
	"endif"  | null ps ->
                    do hPutStrLn stderr $ "Unmatched #endif at "++show p
                       return []
	"endif"  -> skipn syms False (Keep (tail ps)) xs
	"pragma" -> skipn syms True  (Keep ps) xs
        ('!':_)  -> skipn syms False (Keep ps) xs	-- \#!runhs scripts
	"include"-> do (inc,content) <- readFirst (file syms (unwords line))
                                                  p path
                                                  (warnings options)
                       cpp p syms path options (Keep ps)
                             (("#line 1 "++show inc): linesCpp content
                                                    ++ cppline (newline p): xs)
	"warning"-> if warnings options then
                      do hPutStrLn stderr (l++"\nin "++show p)
                         skipn syms False (Keep ps) xs
                    else skipn syms False (Keep ps) xs
	"error"  -> error (l++"\nin "++show p)
	"line"   | all isDigit sym
	         -> (if locations options && hashline options then emitOne (p,l)
                     else if locations options then emitOne (p,cpp2hask l)
                     else id) $
                    cpp (newpos (read sym) (un rest) p)
                        syms path options (Keep ps) xs
	n | all isDigit n && not (null n)
	         -> (if locations options && hashline options then emitOne (p,l)
                     else if locations options then emitOne (p,cpp2hask l)
                     else id) $
	            cpp (newpos (read n) (un (tail ws)) p)
                        syms path options (Keep ps) xs
          | otherwise
	         -> do when (warnings options) $
                           hPutStrLn stderr ("Warning: unknown directive #"++n
                                             ++"\nin "++show p)
                       emitOne (p,l) $
                               cpp (newline p) syms path options (Keep ps) xs

cpp p syms path options (Drop n b ps) (('#':x):xs) =
    let ws = words x
        cmd = if null ws then "" else head ws
        delse    | n==1 && b = Drop 1 b ps
                 | n==1      = Keep ps
                 | otherwise = Drop n b ps
        dend     | n==1      = Keep (tail ps)
                 | otherwise = Drop (n-1) b (tail ps)
        delif s  | n==1 && not b && gatherDefined p syms s
                             = Keep ps
                 | otherwise = Drop n b ps
        skipn ud xs' =
                 let n' = 1 + length (filter (=='\n') x) in
                 emitMany (replicate n' (p,"")) $
                    cpp (newlines n' p) syms path options ud xs'
    in
    if      cmd == "ifndef" ||
            cmd == "if"     ||
            cmd == "ifdef"  then  skipn (Drop (n+1) b (p:ps)) xs
    else if cmd == "elif"   then  skipn (delif (unwords (tail ws))) xs
    else if cmd == "else"   then  skipn  delse xs
    else if cmd == "endif"  then
            if null ps then do hPutStrLn stderr $ "Unmatched #endif at "++show p
                               return []
                       else skipn  dend  xs
    else skipn (Drop n b ps) xs
	-- define, undef, include, error, warning, pragma, line

cpp p syms path options (Keep ps) (x:xs) =
    let p' = newline p in seq p' $
    emitOne (p,x)  $  cpp p' syms path options (Keep ps) xs
cpp p syms path options d@(Drop _ _ _) (_:xs) =
    let p' = newline p in seq p' $
    emitOne (p,"") $  cpp p' syms path options d xs


-- | Auxiliary IO functions
emitOne  ::  a  -> IO [a] -> IO [a]
emitMany :: [a] -> IO [a] -> IO [a]
emitOne  x  io = do ys <- unsafeInterleaveIO io
                    return (x:ys)
emitMany xs io = do ys <- unsafeInterleaveIO io
                    return (xs++ys)


----
gatherDefined :: Posn -> SymTab HashDefine -> String -> Bool
gatherDefined p st inp =
  case papply (parseBoolExp st) inp of
    []      -> error ("Cannot parse #if directive in file "++show p)
    [(b,_)] -> b
    _       -> error ("Ambiguous parse for #if directive in file "++show p)

parseBoolExp :: SymTab HashDefine -> Parser Bool
parseBoolExp st =
  do  a <- parseExp1 st
      skip (string "||")
      b <- first (skip (parseBoolExp st))
      return (a || b)
  +++
      parseExp1 st

parseExp1 :: SymTab HashDefine -> Parser Bool
parseExp1 st =
  do  a <- parseExp0 st
      skip (string "&&")
      b <- first (skip (parseExp1 st))
      return (a && b)
  +++
      parseExp0 st

parseExp0 :: SymTab HashDefine -> Parser Bool
parseExp0 st =
  do  skip (string "defined")
      sym <- parens parseSym
      return (definedST sym st)
  +++
  do  parens (parseBoolExp st)
  +++
  do  skip (char '!')
      a <- parseExp0 st
      return (not a)
  +++
  do  sym1 <- parseSymOrCall st
      op <- parseOp st
      sym2 <- parseSymOrCall st
      return (op (safeRead sym1) (safeRead sym2))
  +++
  do  sym <- parseSymOrCall st
      case safeRead sym of
        0 -> return False
        _ -> return True
  where
    safeRead s =
      case s of
        '0':'x':s' -> number readHex s'
        '0':'o':s' -> number readOct s'
        _          -> number readDec s
    number rd s =
      case rd s of
        []        -> 0 :: Integer
        ((n,_):_) -> n :: Integer

parseOp :: SymTab HashDefine -> Parser (Integer -> Integer -> Bool)
parseOp _ =
  do  skip (string ">=")
      return (>=)
  +++
  do  skip (char '>')
      return (>)
  +++
  do  skip (string "<=")
      return (<=)
  +++
  do  skip (char '<')
      return (<)
  +++
  do  skip (string "==")
      return (==)
  +++
  do  skip (string "!=")
      return (/=)

parseSymOrCall :: SymTab HashDefine -> Parser String
parseSymOrCall st =
  do  sym <- skip parseSym
      args <- parens (parseSymOrCall st `sepby` skip (char ','))
      return (convert sym args)
  +++
  do  sym <- skip parseSym
      return (convert sym [])
  where
    convert sym args =
      case lookupST sym st of
        Nothing  -> sym
        Just (a@SymbolReplacement{}) -> recursivelyExpand st (replacement a)
        Just (a@MacroExpansion{})    -> expandMacro a args False
        Just (a@AntiDefined{})       -> name a

recursivelyExpand :: SymTab HashDefine -> String -> String
recursivelyExpand st inp =
  case papply (parseSymOrCall st) inp of
    [(b,_)] -> b
    _       -> inp

parseSym :: Parser String
parseSym = many1 (alphanum+++char '\''+++char '`')

parens p = bracket (skip (char '(')) (skip p) (skip (char ')'))

-- | Determine filename in \#include
file :: SymTab HashDefine -> String -> String
file st name =
    case name of
      ('"':ns) -> init ns
      ('<':ns) -> init ns
      _ -> let ex = recursivelyExpand st name in
           if ex == name then name else file st ex
