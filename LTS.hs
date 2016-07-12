-------------------------------------------------------------------------------
-- LTS.hs
-- Labeled Transition System (LTS) library
-- Ramy Shahin - July 10th 2016
-------------------------------------------------------------------------------
module LTS where
import Data.Graph
import Data.List

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
    getInitStates :: [Vertex] 
    -- TODO: [AP] 
    -- TODO: (Table [AP])
    } deriving (Show)

-- fine-grained accessors
getStates :: LTS -> [Vertex]
getStates = vertices . getGraph

getTransitions :: LTS -> [Edge]
getTransitions = edges . getGraph

-------------------------------------------------------------------------------
-- LTS Algorithms
-------------------------------------------------------------------------------

-- reachability
isReachable :: LTS -> Vertex -> Bool
isReachable lts s = any inTree forest
    where   inTree      = elem s
            forest      = dfs graph initStates
            graph       = getGraph lts
            initStates  = getInitStates lts
            
-- reachability with witness path
dfs_ :: [Edge] -> [Vertex] -> Vertex -> Vertex -> [Vertex]
dfs_ edges path target src =
    if src == target 
    then path ++ [target]
    else let    neighbors = [x | (s, x) <- edges, s == src] 
                unvisitedNeighbors = neighbors \\ path
                paths = map (dfs_ edges (path ++ [src]) target) unvisitedNeighbors 
                nonEmptyPaths = filter (not . null) paths in 
                    case nonEmptyPaths of
                       [] -> []
                       (x:xs) -> x
                        
witnessPath :: LTS -> Vertex -> [Vertex]
witnessPath lts s = 
    let states      = getStates lts
        transitions = getTransitions lts
        initStates  = getInitStates lts
        paths       = map (dfs_ transitions [] s) initStates
        nonEmptyPaths = filter (not . null) paths in 
            case nonEmptyPaths of
                [] -> []
                (x:xs) -> x
                
        