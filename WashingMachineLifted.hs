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

states :: [State']
states = [start, locking, waiting, washing, drying, unlocking, finish]

heaterOn, heaterOff, washStart, tempCheck, setDelay, quickCool :: Action'
heaterOn    = mkVar "heaterOn" heat
heaterOff   = mkVar "heaterOff" heat
washStart   = mkVar "washStart" T
tempCheck   = mkVar "tempCheck" T
setDelay    = mkVar "setDelay" delay
quickCool   = mkVar "quickCool" T

actions :: [Action']
actions =  [heaterOn, heaterOff, washStart, tempCheck, setDelay, quickCool]

heatEnabled, delayEnabled :: Guard'
heatEnabled = mkVar "heatEnabled" heat
delayEnabled = mkVar "delayEnabled" delay

guards :: [Guard']
guards = [heatEnabled, delayEnabled]

start2locking, locking2waiting :: Transition'

--                                           from         to       guards        actions
start2locking       = mkVar (Transition     start        locking  []               [])  T
locking2waiting     = mkVar (Transition     locking      waiting  [heatEnabled]    [heaterOn]) (disj[heat, delay])
waiting2washing     = mkVar (Transition     waiting      washing  []               [heaterOff, washStart]) (disj[heat, delay])
washing2unlocking   = mkVar (Transition     washing      unlocking []              [quickCool]) (neg dry)
washing2drying      = mkVar (Transition     washing      drying       []                []) dry
drying2unlocking    = mkVar (Transition     drying       unlocking    []           [quickCool]) dry
unlocking2finish    = mkVar (Transition     unlocking    finish       []                []) T

transitions :: [Transition']

transitions =  [start2locking, locking2waiting, waiting2washing, washing2unlocking, washing2drying, drying2unlocking, unlocking2finish]

washingMachine :: LTS'
washingMachine = mkVar (LTS  states  guards  actions  transitions  [start]) T
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