module Rewrite.Case where

import Rewrite.Common
import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Ann 
import Language.Haskell.Tools.Rewrite.Match.Decls

import Control.Reference -- ((.-), (.=), (^.) (&))
import FastString
import Debug.Trace
import Data.Char
import qualified Data.Set as S 
import qualified SPL as L
import Rewrite.Base
import Rewrite.Deep

