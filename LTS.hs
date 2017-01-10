-------------------------------------------------------------------------------
-- LTS.hs
-- Labeled Transition System (LTS) library
-- Ramy Shahin - July 10th 2016
-------------------------------------------------------------------------------
module LTS where
import Data.List   as L
import Data.Vector as V

-- abstract State type
type State = Int

-- abstract Action type
type Act = Int

-- abstract Transition type
data Transition = Transition {
    source  :: State,
    target  :: State,
    actions :: [Act]
    } deriving (Show)

-- abstract abstract proposition type
type AP = Int

-- an LTS a tuple <S, Act, ->, I, AP, L> where: 
--      S is a set of states (graph nodes)
--      Act is a set of actions (labels for edges)
--      -> is a transition relation (graph edges)
--      I is the set of initial states (subset of S)
--      TODO: AP is a set of atomic propositions
--      TODO: L is a a labeling function, mapping states to sets of propositions (AP)
data LTS = LTS {
    getStates       :: Vector State,
    getActions      :: Vector Act,
    getTransitions  :: Vector Transition,
    getInitStates   :: [Int] 
    -- TODO: [AP] 
    -- TODO: (Table [AP])
    } deriving (Show)

 
-------------------------------------------------------------------------------
-- LTS Algorithms
-------------------------------------------------------------------------------

neighbors :: Vector Transition -> State -> [State]
neighbors ts' s = 
    if V.null ts' then []
    else
          let  t = V.head ts'
               ts = V.tail ts'
          in
               if ((source t) == s)
               then (target t) : neighbors ts s
               else neighbors ts s

-- Depth-first Search
dfs ::  Vector Transition    ->      -- graph edges
        [State]              ->      -- visited states
        State                ->      -- target node
        State                ->      -- source node
        [State]                      -- returns the path from source to target
        
dfs edges visited target src =
    let visited' = visited L.++ [src]
    in  if (target == src) then visited'
        else let ns = (neighbors edges src) \\ visited'
             in L.head (L.map (\n -> let r = (dfs edges visited' target n)
                                 in  if (L.null r) then [] else (src : r)) ns)
    
-- reachability
isReachable ::  LTS     ->  -- input LTS 
                State  ->   -- state to check if reachable 
                Bool        -- returns True iff input state is reachable
                
isReachable lts s = L.any (not. L.null) paths
    where   paths       = L.map (dfs transitions [] s) initStates
            transitions = getTransitions lts
            initStates  = getInitStates lts
           

-- reachability with witness path                       
witnessPath ::  LTS     ->      -- input LTS
                State   ->      -- state to find a witness path to
                [State]         -- returns a path or an empty list of none exist
                
witnessPath lts s = 
    let states      = getStates lts
        transitions = getTransitions lts
        initStates  = getInitStates lts
        paths       = L.map (dfs transitions [] s) initStates
        nonEmptyPaths = L.filter (not . L.null) paths in 
            case nonEmptyPaths of
                [] -> []
                (x:xs) -> x
                