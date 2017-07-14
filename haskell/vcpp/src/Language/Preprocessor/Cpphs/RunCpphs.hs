{-
-- The main program for cpphs, a simple C pre-processor written in Haskell.

-- Copyright (c) 2004 Malcolm Wallace
-- This file is LGPL (relicensed from the GPL by Malcolm Wallace, October 2011).
-}
module Language.Preprocessor.Cpphs.RunCpphs ( runCpphs
                                            , runCpphsReturningSymTab
                                            ) where

import Language.Preprocessor.Cpphs.CppIfdef (cppIfdef)
import Language.Preprocessor.Cpphs.MacroPass(macroPass,macroPassReturningSymTab)
import Language.Preprocessor.Cpphs.Options  (CpphsOptions(..), BoolOptions(..))
import Language.Preprocessor.Cpphs.Tokenise (deWordStyle, tokenise)
import Language.Preprocessor.Unlit as Unlit (unlit)


runCpphs :: CpphsOptions -> FilePath -> String -> IO String
runCpphs options filename input = do
  let bools  = boolopts options
      preInc = case preInclude options of
                 [] -> ""
                 is -> concatMap (\f->"#include \""++f++"\"\n") is 
                       ++ "#line 1 \""++filename++"\"\n"

  pass1 <- cppIfdef filename (defines options) (includes options) bools
                    (preInc++input)
  pass2 <- macroPass (defines options) bools pass1
  let result= if not (macros bools)
              then if   stripC89 bools || stripEol bools
                   then concatMap deWordStyle $
                        tokenise (stripC89 bools) (stripEol bools)
                                 (ansi bools) (lang bools) pass1
                   else unlines (map snd pass1)
              else pass2
      pass3 = if literate bools then Unlit.unlit filename else id

  return (pass3 result)

runCpphsReturningSymTab :: CpphsOptions -> FilePath -> String
             -> IO (String,[(String,String)])
runCpphsReturningSymTab options filename input = do
  let bools  = boolopts options
      preInc = case preInclude options of
                 [] -> ""
                 is -> concatMap (\f->"#include \""++f++"\"\n") is 
                       ++ "#line 1 \""++filename++"\"\n"

  pass1 <- cppIfdef filename (defines options) (includes options) bools
                    (preInc++input)
  (pass2,syms) <- macroPassReturningSymTab (defines options) bools pass1
  let result= if not (macros bools)
              then if   stripC89 bools || stripEol bools
                   then concatMap deWordStyle $
                        tokenise (stripC89 bools) (stripEol bools)
                                 (ansi bools) (lang bools) pass1
                   else unlines (map snd pass1)
              else pass2
      pass3 = if literate bools then Unlit.unlit filename else id

  return (pass3 result, syms)

