-- Wiper.hs
-- Ramy Shahin
-- March 19th 2017
module Wiper where

import LTS
import LiftedLTS
import Prop
import SPL
import Test.HUnit (Test(..), assertEqual, runTestTT)
import Control.Applicative

-- Features
f :: Universe
f = mkUniverse ["sL", "sH", "wL", "wH"]

sL, sH, wL, wH :: Prop
sL  = Atom f 0
sH  = Atom f 1
wL  = Atom f 2
wH  = Atom f 3

states :: Var [State]
states = mkVarStates 0 2

non, noRain, little, heavy, rain, heavyRain :: Action'
non         = mkAction (mkVarT "non") e
noRain      = mkAction (mkVarT "noRain") e
little      = mkAction (mkVarT "little") e
heavy       = mkAction (mkVarT "heavy") e
rain        = mkAction (mkVarT "rain") e
heavyRain   = mkAction (mkVarT "heavyRain") e


actions :: Var [Action]
actions =  mkVarList [non, noRain, little, heavy, rain, heavyRain]

start2locking, locking2waiting :: Transition'

--                                     from         to          actions
s0_s1_euro          = mkTransition     (states!!0)  (states!!1)  e -- T
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

