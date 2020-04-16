-- macro.hs
--  a module for processing C preprocessor macros
--  Ramy Shahin
--  March 2nd 2017
{-
module Macro where
import SPL
import Data.Map

data Macro = Macro {
    name        :: String,
    params      :: [String],
    bodyTokens  :: [String]
    expansion   :: [String] -> String
} deriving (Show)

type Macro' = Var Macro

type MacroTable = String (Map Macro')

addMacro :: PresenceCondition -> MacroTable -> Macro -> MacroTable
-- TODO: #define

removeMacro :: PresenceCondition -> MacroTable -> String -> MacroTable
-- TODO: #undef

-}



