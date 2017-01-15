-------------------------------------------------------------------------------
-- LiftedLTS.hs
-- Lifted Labeled Transition System (LTS) library
-- Ramy Shahin - July 20th 2016
-------------------------------------------------------------------------------
module LiftedLTS where
import LTS
import Data.List
import Data.Tree
import Control.Applicative
import SPL

type State' = Var State
type Guard' = Var Guard
type Action' = Var Action

-- abstract Transition type
--data Transition = Transition {
--    source   :: State',
--    target   :: State',
--    guardBy  :: [Guard'],
--    act      :: [Action']
--    } --deriving (Show)

source' = liftA source
target' = liftA target

type Transition' = Var Transition

--data LTS = LTS {
--    getStates'       :: [State'],
--    getGuards'       :: [Guard'],
--    getActions'      :: [Action'],
--    getTransitions'  :: [Transition'],
--    getInitStates'   :: [State'] 
--    -- TODO: [AP] 
--    -- TODO: (Table [AP])
--    } --deriving (Show)

type LTS' = Var LTS

neighbors' :: Var [Transition'] -> State' -> Var [State']
neighbors' ts' s = 
    cond' (null' ts') e
    (
          let  t = head' ts'
               ts = tail' ts'
          in
               cond' ((source' t) |==| s)
                     ((target' t) |:| neighbors' ts s)
                     (neighbors' ts s)
    )