-------------------------------------------------------------------------------
-- LTS.hs
-- Labeled Transition System (LTS) library
-- Ramy Shahin - July 10th 2016
-------------------------------------------------------------------------------
{-# LANGUAGE ExistentialQuantification #-}

module LTS where
import Data.List
--import Data.Vector as V

-- abstract State type
--type State = Int

-- abstract Action type
--type Act = Int

--type Guard = Int

-- abstract Transition type
data Transition s g a = Eq s => Transition {
    source  :: s,
    target  :: s,
    guardBy :: [g],
    act     :: [a]
    } --deriving (Show)

-- abstract abstract proposition type
--type AP = Int

-- an LTS a tuple <S, Act, ->, I, AP, L> where: 
--      S is a set of states (graph nodes)
--      Act is a set of actions (labels for edges)
--      -> is a transition relation (graph edges)
--      I is the set of initial states (subset of S)
--      TODO: AP is a set of atomic propositions
--      TODO: L is a a labeling function, mapping states to sets of propositions (AP)
data LTS s g a = Eq s => LTS {
    getStates       :: [s],
    getGuards       :: [g],
    getActions      :: [a],
    getTransitions  :: [Transition s a g],
    getInitStates   :: [s] 
    -- TODO: [AP] 
    -- TODO: (Table [AP])
    } --deriving (Show)

 
-------------------------------------------------------------------------------
-- LTS Algorithms
-------------------------------------------------------------------------------

neighbors :: Eq s => [Transition s g a] -> s -> [s]
neighbors ts' s = 
    if null ts' then []
    else
          let  t = head ts'
               ts = tail ts'
          in
               if ((source t) == s)
               then (target t) : neighbors ts s
               else neighbors ts s

-- Depth-first Search
dfs ::  Eq s =>
        [Transition s g a]  ->      -- graph edges
        [s]                 ->      -- visited states
        s                   ->      -- target node
        s                   ->      -- source node
        [s]                         -- returns the path from source to target
        
dfs edges visited target src =
    let visited' = visited ++ [src]
    in  if (target == src) then visited'
        else let ns = (neighbors edges src) \\ visited'
             in head (map (\n -> let r = (dfs edges visited' target n)
                                 in  if (null r) then [] else (src : r)) ns)
    
-- reachability
isReachable ::  Eq s =>
                LTS s g a   ->  -- input LTS 
                s           ->  -- state to check if reachable 
                Bool            -- returns True iff input state is reachable
                
isReachable lts s = any (not. null) paths
    where   paths       = map (dfs transitions [] s) initStates
            transitions = getTransitions lts
            initStates  = getInitStates lts
           

-- reachability with witness path                       
witnessPath ::  Eq s =>
                LTS s g a   ->      -- input LTS
                s           ->      -- state to find a witness path to
                [s]                 -- returns a path or an empty list of none exist
                
witnessPath lts s = 
    let states      = getStates lts
        transitions = getTransitions lts
        initStates  = getInitStates lts
        paths       = map (dfs transitions [] s) initStates
        nonEmptyPaths = filter (not . null) paths in 
            case nonEmptyPaths of
                [] -> []
                (x:xs) -> x
                