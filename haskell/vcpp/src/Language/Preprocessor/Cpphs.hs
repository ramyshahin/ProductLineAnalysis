-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Preprocessor.Cpphs
-- Copyright   :  2000-2006 Malcolm Wallace
-- Licence     :  LGPL
--
-- Maintainer  :  Malcolm Wallace <Malcolm.Wallace@cs.york.ac.uk>
-- Stability   :  experimental
-- Portability :  All
--
-- Include the interface that is exported
-----------------------------------------------------------------------------

module Language.Preprocessor.Cpphs
  ( runCpphs, runCpphsReturningSymTab
  , cppIfdef
  , macroPass, macroPassReturningSymTab
  , CpphsOptions(..), BoolOptions(..)
  , parseOptions, defaultCpphsOptions, defaultBoolOptions
  , module Language.Preprocessor.Cpphs.Position
  ) where

import Language.Preprocessor.Cpphs.CppIfdef(cppIfdef)
import Language.Preprocessor.Cpphs.MacroPass(macroPass
                                            ,macroPassReturningSymTab)
import Language.Preprocessor.Cpphs.RunCpphs(runCpphs
                                           ,runCpphsReturningSymTab)
import Language.Preprocessor.Cpphs.Options
       (CpphsOptions(..), BoolOptions(..), parseOptions
       ,defaultCpphsOptions,defaultBoolOptions)
import Language.Preprocessor.Cpphs.Position