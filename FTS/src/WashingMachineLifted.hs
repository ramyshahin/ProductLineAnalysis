module WashingMachineLifted where
import LTS
import LiftedLTS
import Prop
import SPL
import Test.HUnit (Test(..), assertEqual, runTestTT)
import Control.Applicative

-- Features
f :: Universe
f = mkUniverse ["heat", "delay", "dry"]

heat, delay, dry :: Prop
heat  = Atom f 0
delay = Atom f 1
dry   = Atom f 2

start, locking, waiting, washing, drying, unlocking, finish :: State'
start       = mkVar "start" T
locking     = mkVar "locking" T
waiting     = mkVar "waiting" (disj[heat, delay])
washing     = mkVar "washing" T
drying      = mkVar "drying" dry
unlocking   = mkVar "unlocking" T
finish      = mkVar "finish" T

states :: Var [State]
states = mkVarList [start, locking, waiting, washing, drying, unlocking, finish]

heaterOn, heaterOff, washStart, tempCheck, setDelay, quickCool :: Action'
heaterOn    = mkAction (mkVar "heaterOn" heat) (mkVarList [heatEnabled, delayEnabled])
heaterOff   = mkAction (mkVar "heaterOff" heat) e
washStart   = mkAction (mkVar "washStart" T) e
tempCheck   = mkAction (mkVar "tempCheck" heat) (mkVarList [entry])
setDelay    = mkAction (mkVar "setDelay" delay) e
quickCool   = mkAction (mkVar "quickCool" T) e

actions :: Var [Action]
actions =  mkVarList [heaterOn, heaterOff, washStart, tempCheck, setDelay, quickCool]

heatEnabled, delayEnabled, entry :: Guard'
entry = mkVar "entry" T
heatEnabled = mkVar "heatEnabled" heat
delayEnabled = mkVar "delayEnabled" delay

start2locking, locking2waiting :: Transition'

--                                     from         to       actions
start2locking       = mkTransition     start        locking  e -- T
locking2waiting     = mkTransition     locking      waiting  (mkVarList [heaterOn]) -- (disj[heat, delay])
locking2washing     = mkTransition     locking      washing  (mkVarList [setDelay, washStart])
waiting2washing     = mkTransition     waiting      washing  (mkVarList [heaterOff, washStart]) -- (disj[heat, delay])
washing2unlocking   = mkTransition     washing      unlocking (mkVarList [quickCool]) -- (neg dry)
washing2drying      = mkTransition     washing      drying    e -- dry
drying2unlocking    = mkTransition     drying       unlocking (mkVarList [quickCool]) -- dry
unlocking2finish    = mkTransition     unlocking    finish    e -- T

transitions :: Var [Transition]

transitions =  mkVarList [start2locking, locking2waiting, locking2washing, waiting2washing, washing2unlocking, washing2drying, drying2unlocking, unlocking2finish]

washingMachine :: LTS'
washingMachine = mkLTS  states  actions  transitions  (mkVarList [start]) --T

{-
-- neighborsTests
neighborsStart      = TestCase (assertEqual "neighborsStart"        (neighbors'  transitions  start)       [locking])
neighborsLocking    = TestCase (assertEqual "neighborsLocking"      (neighbors'  transitions  locking)     [waiting])
neighborsWaiting    = TestCase (assertEqual "neighborsWaiting"      (neighbors'  transitions  waiting)     [washing])
neighborsWashing    = TestCase (assertEqual "neighborsWashing"      (neighbors'  transitions  washing)     [unlocking, drying])
neighborsDrying     = TestCase (assertEqual "neighborsDrying"       (neighbors'  transitions  drying)      [unlocking])
neighborsUnlocking  = TestCase (assertEqual "neighborsUnlocking"    (neighbors'  transitions  unlocking)   [finish])
neighborsFinish     = TestCase (assertEqual "neighborsFinish"       (neighbors'  transitions  finish)      (mkVar [([], T)]))

neighborsTests = TestList [neighborsStart, neighborsLocking, neighborsWaiting, neighborsWashing, 
                  neighborsDrying, neighborsUnlocking, neighborsFinish]
                  -}