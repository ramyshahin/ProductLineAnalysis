-------------------------------------------------------------------------------
-- LTS.hs
-- Labeled Transition System (LTS) library
-- Ramy Shahin - July 10th 2016
-------------------------------------------------------------------------------
module LTS where
import Data.Graph
import Data.List
import Variability

-- abstract State type
type State = Vertex

-- abstract Transition type
type Transition = Edge

-- abstract Action type
type Act = Int

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
    getGraph    :: Graph,
    getActions  :: [Act],
    getInitStates :: [State] 
    -- TODO: [AP] 
    -- TODO: (Table [AP])
    } deriving (Show)

-- fine-grained accessors
getStates :: LTS -> [State]
getStates = vertices . getGraph

getTransitions :: LTS -> [Transition]
getTransitions = edges . getGraph

-------------------------------------------------------------------------------
-- LTS Algorithms
-------------------------------------------------------------------------------

-- Depth-first Search
dfs_ :: [Transition]    ->      -- graph edges
        [State]         ->      -- path prefix so far from source (recursively computed)
        State           ->      -- target node
        State           ->      -- source node 
        [State]                 -- returns the path from source to target
        
dfs_ edges pathPrefix target src =
    if src == target 
    then pathPrefix ++ [target]
    else let    neighbors = [x | (s, x) <- edges, s == src] 
                unvisitedNeighbors = neighbors \\ pathPrefix
                paths = map (dfs_ edges (pathPrefix ++ [src]) target) unvisitedNeighbors 
                nonEmptyPaths = filter (not . null) paths in 
        case nonEmptyPaths of
                [] -> []
                (x:xs) -> x
                       
-- reachability
isReachable ::  LTS     ->  -- input LTS 
                State  ->   -- state to check if reachable 
                Bool        -- returns True iff input state is reachable
                
isReachable lts s = any (not. null) paths
    where   paths       = map (dfs_ transitions [] s) initStates
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
        paths       = map (dfs_ transitions [] s) initStates
        nonEmptyPaths = filter (not . null) paths in 
            case nonEmptyPaths of
                [] -> []
                (x:xs) -> x
                
-------------------------------------------------------------------------------
-- LTS Variability Algorithms
-------------------------------------------------------------------------------
type SPLState = SPLVariable State
type SPLTransition = SPLVariable (SPLState, SPLState)

root :: Tree a -> a
root (Node r _) = r

children :: Tree a -> Forest a
children (Node _ f) = f

-- collapse function for result
collapse :: Forest SPLState -> [SPLState] -> Forest SPLState

collapse f l = 
    let roots = map root f in 
        case l of 
            []   -> f
            x:xs -> if (elem x roots)
                    then map (\n@(Node r cs) -> if r == x then (Node r (collapse cs xs)) else n) f
                    else (Node x (collapse [] xs)) : f 
                    
-- Variability Depth-first Search
vdfs_ ::    SPLContext         ->      -- SPL variability context
            [SPLTransition]    ->      -- graph edges
            [SPLState]         ->      -- path prefix so far from source (recursively computed)
            SPLState           ->      -- target node
            SPLState           ->      -- source node 
            [SPLState]                 -- returns the path from source to target
        
vdfs_ cntxt edges pathPrefix target src =
    if (get cntxt src) == (get cntxt target) 
    then pathPrefix ++ [target]
    else let    neighbors = [SPLVariable x (condE && condT) | SPLVariable (s, SPLVariable x condT) condE <- edges, s == src] 
                unvisitedNeighbors = neighbors \\ pathPrefix
                paths = map (vdfs_ cntxt edges (pathPrefix ++ [src]) target) unvisitedNeighbors 
                nonEmptyPaths = filter (not . null) paths in 
        case nonEmptyPaths of
                [] -> []
                (x:xs) -> x
    