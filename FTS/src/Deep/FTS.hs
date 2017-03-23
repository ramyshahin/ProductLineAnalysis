-------------------------------------------------------------------------------
-- LiftedLTS.hs
-- Lifted Labeled Transition System (LTS) library
-- Ramy Shahin - July 20th 2016
-------------------------------------------------------------------------------
module Deep.FTS where
import SPL
import Data.List

type State' = String
type State = Var State'

type Guard = String

mkStates :: Int -> Int -> [State]
mkStates begin end =
    if begin > end then [] else s : mkStates (begin + 1) end
    where s = mkVarT ("s" ++ (show begin))

data Action = Action {
    action :: String,
    guards :: [Guard]
    } deriving (Eq, Show)

-- abstract Transition type
data Transition' = Transition' {
    source' :: State',
    target' :: State',
    act'    :: [Action]
    } deriving (Show)
type Transition = Var Transition'

mkTransition :: State -> State -> VList Action -> PresenceCondition -> Transition
mkTransition source target act pc = restrict pc $ (liftV3 Transition') source target act 
source = liftV source'
target = liftV target'
act    = liftV act'

-- abstract abstract proposition type
--type AP = Int

-- an LTS a tuple <S, Act, ->, I, AP, L> where: 
--      S is a set of states (graph nodes)
--      Act is a set of actions (labels for edges)
--      -> is a transition relation (graph edges)
--      I is the set of initial states (subset of S)
--      TODO: AP is a set of atomic propositions
--      TODO: L is a a labeling function, mapping states to sets of propositions (AP)
data FTS' = FTS' {
    getStates       :: [State'],
    getActions      :: [Action],
    getTransitions  :: [Transition'],
    getInitStates   :: [State'] 
    -- TODO: [AP] 
    -- TODO: (Table [AP])
    } deriving (Show)
type FTS = Var FTS'

mkFTS :: VList State' -> VList Action -> VList Transition' -> VList State' -> PresenceCondition -> FTS
mkFTS states actions transitions initStates pc = restrict pc $ (liftV4 FTS') states actions transitions initStates

-------------------------------------------------------------------------------
-- LTS Algorithms
-------------------------------------------------------------------------------

neighbors :: VList Transition' -> State -> VList State'
neighbors ts' s = 
    cond'   (vnull ts') 
            e
            (let    t = vhead ts'
                    ts = vtail ts'
             in
                    cond' ((source t) |==| s)
                        ((target t) |:| neighbors ts s)
                        (neighbors ts s)
            )