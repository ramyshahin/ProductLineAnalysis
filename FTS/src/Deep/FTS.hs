-------------------------------------------------------------------------------
-- LiftedLTS.hs
-- Lifted Labeled Transition System (LTS) library
-- Ramy Shahin - July 20th 2016
-------------------------------------------------------------------------------
module Deep.FTS where
import SPL
import Deep.VList
import LTS(Action)

type State' = String
type VState = Var State'

--type Guard = String

mkStates :: Int -> Int -> [VState]
mkStates begin end =
    if begin > end then [] else s : mkStates (begin + 1) end
    where s = mkVarT ("s" ++ (show begin))

--data VAction = VAction {
--    action :: String,
--    guards :: [Guard]
--    } deriving (Eq, Show)

-- abstract Transition type
data Transition' = Transition' {
    source :: VState,
    target :: VState,
    act    :: VList Action
    } deriving (Show)
type VTransition = Var Transition'

--mkTransition :: State -> State -> VList Action -> PresenceCondition -> Transition
--mkTransition source target act pc = restrict pc $ (liftV3 Transition') source target act 
--source = liftV source'
--target = liftV target'
--act    = liftV act'

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
    getStates       :: VList State',
    getActions      :: VList Action,
    getTransitions  :: VList Transition',
    getInitStates   :: VList State' 
    -- TODO: [AP] 
    -- TODO: (Table [AP])
    } deriving (Show)
type FTS = Var FTS'

mkFTS :: VList State' -> VList Action -> VList Transition' -> VList State' -> PresenceCondition -> FTS
mkFTS states actions transitions initStates pc = restrict pc $ (liftV4 FTS') states actions transitions initStates

-------------------------------------------------------------------------------
-- LTS Algorithms
-------------------------------------------------------------------------------

neighbors :: VList Transition' -> VState -> VList State'
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