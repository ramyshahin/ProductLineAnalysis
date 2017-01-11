import LiftedLTS
import Prop
import SPL
import Test.HUnit (Test(..), assertEqual, runTestTT)

-- Features
f :: Universe
f = mkUniverse ["heat", "delay", "dry"]

heat, delay, dry :: Prop
heat  = Atom f 0
delay = Atom f 1
dry   = Atom f 2

type State = String
type Guard = String
type Action = String

type State' = Var State
type Guard' = Var Guard
type Action' = Var Action

start, locking, waiting, washing, drying, unlocking, finish :: State'
start       = mkVar [("start", T)]
locking     = mkVar [("locking", T)]
waiting     = mkVar [("waiting", Disj[heat, delay])]
washing     = mkVar [("washing", T)]
drying      = mkVar [("drying", dry)]
unlocking   = mkVar [("unlocking", T)]
finish      = mkVar [("finish", T)]

states :: [State']
states = [start, locking, waiting, washing, drying, unlocking, finish]

heaterOn, heaterOff, washStart, tempCheck, setDelay, quickCool :: Action'
heaterOn    = [("heaterOn", heat)]
heaterOff   = [("heaterOff", heat)]
washStart   = [("washStart", T)]
tempCheck   = [("tempCheck", T)]
setDelay    = [("setDelay", delay)]
quickCool   = [("quickCool", T)]

actions :: [Action']
actions =  [heaterOn, heaterOff, washStart, tempCheck, setDelay, quickCool]

heatEnabled, delayEnabled :: Guard'
heatEnabled = [("heatEnabled", heat)]
delayEnabled = [("delayEnabled", delay)]

guards :: [Guard']
guards = [heatEnabled, delayEnabled]

transitions :: [Transition' State' Guard' Action']
--                                      from            to              guards              actions
transitions =  [(consTransition' <*>    start       <*> locking     <*> []              <*> []),
                (consTransition' <*>    locking     <*> waiting     <*> [heatEnabled]   <*> [heaterOn]),
                (consTransition' <*>    waiting     <*> washing     <*> []              <*> [heaterOff, washStart]),
                (consTransition' <*>    washing     <*> unlocking   <*> []              <*> [quickCool]),
                (consTransition' <*>    washing     <*> drying      <*> []              <*> []),
                (consTransition' <*>    drying      <*> unlocking   <*> []              <*> [quickCool]),
                (consTransition' <*>    unlocking   <*> finish      <*> []              <*> [])
               ]

washingMachine :: LTS' State' Guard' Action'
washingMachine = LTS' states guards actions transitions [start]

-- neighborsTests
neighborsStart      = TestCase (assertEqual "neighborsStart"        (neighbors' <*> transitions <*> start)       [locking])
neighborsLocking    = TestCase (assertEqual "neighborsLocking"      (neighbors' <*> transitions <*> locking)     [waiting])
neighborsWaiting    = TestCase (assertEqual "neighborsWaiting"      (neighbors' <*> transitions <*> waiting)     [washing])
neighborsWashing    = TestCase (assertEqual "neighborsWashing"      (neighbors' <*> transitions <*> washing)     [unlocking, drying])
neighborsDrying     = TestCase (assertEqual "neighborsDrying"       (neighbors' <*> transitions <*> drying)      [unlocking])
neighborsUnlocking  = TestCase (assertEqual "neighborsUnlocking"    (neighbors' <*> transitions <*> unlocking)   [finish])
neighborsFinish     = TestCase (assertEqual "neighborsFinish"       (neighbors' <*> transitions <*> finish)      (mkVar [([], T)]))

neighborsTests = TestList [neighborsStart, neighborsLocking, neighborsWaiting, neighborsWashing, 
                  neighborsDrying, neighborsUnlocking, neighborsFinish]