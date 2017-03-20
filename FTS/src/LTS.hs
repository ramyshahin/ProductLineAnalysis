-------------------------------------------------------------------------------
-- LTS.hs
-- Labeled Transition System (LTS) library
-- Ramy Shahin - July 10th 2016
-------------------------------------------------------------------------------
{-# LANGUAGE ExistentialQuantification #-}

module LTS where
import Data.List

type State = String
type Guard = String

mkStates :: Int -> Int -> [State]
mkStates begin end =
    if begin > end then [] else s : mkStates (begin + 1) end
    where s = "s" ++ (show begin)
     
data Action = Action {
    action :: String,
    guards :: [Guard]
    } deriving (Eq, Show)

-- abstract Transition type
data Transition = Transition {
    source  :: State,
    target  :: State,
    act     :: [Action]
    } deriving (Eq, Show)

-- abstract abstract proposition type
--type AP = Int

-- an LTS a tuple <S, Act, ->, I, AP, L> where: 
--      S is a set of states (graph nodes)
--      Act is a set of actions (labels for edges)
--      -> is a transition relation (graph edges)
--      I is the set of initial states (subset of S)
--      TODO: AP is a set of atomic propositions
--      TODO: L is a a labeling function, mapping states to sets of propositions (AP)
data LTS = LTS {
    getStates       :: [State],
    getActions      :: [Action],
    getTransitions  :: [Transition],
    getInitStates   :: [State] 
    -- TODO: [AP] 
    -- TODO: (Table [AP])
    } deriving (Eq, Show)

 
-------------------------------------------------------------------------------
-- LTS Algorithms
-------------------------------------------------------------------------------

neighbors :: [Transition] -> State -> [State]
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
dfs ::  [Transition]        ->      -- graph edges
        [State]             ->      -- visited states
        State               ->      -- target node
        State               ->      -- source node
        [State]                     -- returns the path from source to target
        
dfs edges visited target src =
    let visited' = visited ++ [src]
    in  if (target == src) then visited'
        else let    ns = (neighbors edges src) \\ visited'
                    r' = filter (not . null) (map (\n -> dfs edges visited' target n) ns)
             in if (null r') then [] else head r'
    
-- reachability
isReachable ::  LTS    ->  -- input LTS 
                State  ->  -- state to check if reachable 
                Bool       -- returns True iff input state is reachable
                
isReachable lts s = any (not. null) paths
    where   paths       = map (dfs transitions [] s) initStates
            transitions = getTransitions lts
            initStates  = getInitStates lts
           

-- reachability with witness path                       
witnessPath ::  LTS   ->      -- input LTS
                State ->      -- state to find a witness path to
                [State]       -- returns a path or an empty list of none exist
                
witnessPath lts s = 
    let states      = getStates lts
        transitions = getTransitions lts
        initStates  = getInitStates lts
        paths       = map (dfs transitions [] s) initStates
        nonEmptyPaths = filter (not . null) paths in 
            case nonEmptyPaths of
                [] -> []
                (x:xs) -> x
                