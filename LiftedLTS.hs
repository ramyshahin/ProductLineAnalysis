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
import Debug.Trace
import Control.Exception

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

type Transition' = Var Transition

mkTransition = cliftV4 Transition
source' = cliftV source
target' = cliftV target
guardBy' = cliftV guardBy
act' = cliftV act

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

mkLTS = cliftV5 LTS

--neighbors ts' s = 
--    if null ts' then []
--    else
--          let  t = head ts'
--               ts = tail ts'
--          in
--               if ((source t) == s)
--               then (target t) : neighbors ts s
--               else neighbors ts s

neighbors' :: Var [Transition] -> State' -> Var [State]
neighbors' ts s = 
    cond'   (null' ts) 
            e
            (
                let t' = head' ts
                    ts' = tail' ts
                in  cond'   ((source' t') |==| s)
                            ((target' t') |:| (neighbors' ts' s))
                            (neighbors' ts' s)
            )