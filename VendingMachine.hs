-- VendingMachine.hs
-- Ramy Shahin
-- March 17th 2017
module VendingMachine where

import LTS
import LiftedLTS
import Prop
import SPL
import Test.HUnit (Test(..), assertEqual, runTestTT)
import Control.Applicative

-- Features
f :: Universe
f = mkUniverse ["coffee", "tea", "cappuccino", "ringTone", "euro", "dollar"]

coffee, tea, cappuccino, ringTone, euro, dollar :: Prop
coffee      = Atom f 0
tea         = Atom f 1
cappuccino  = Atom f 2
ringTone    = Atom f 3
euro        = Atom f 4
dollar      = Atom f 5

states :: Var [State]
states = mkVarStates 0 13

oneEuro, oneDollar, sugar, noSugar, coffee_, tea_, cappuccino_, pourSugar, pourCoffee, pourTea, pourMilk, displayDone, skipRingTone, ringTone, cupTaken :: Action'
oneEuro     = mkAction (mkVarT "oneEuro") e
oneDollar   = mkAction (mkVarT "oneDollar") e
sugar       = mkAction (mkVarT "sugar") e
noSugar     = mkAction (mkVarT "noSugar") e
coffee_     = mkAction (mkVarT "coffee_") e
tea_        = mkAction (mkVarT "tea_") e
cappuccino_ = mkAction (mkVarT "cappuccino_") e
pourSugar   = mkAction (mkVarT "pourSugar") e
pourCoffee  = mkAction (mkVarT "pourCoffee") e
pourTea     = mkAction (mkVarT "pourTea") e
pourMilk    = mkAction (mkVarT "pourMilk") e
displayDone = mkAction (mkVarT "displayDone") e
skipRingTone = mkAction (mkVarT "skipRingTone") e
ringTone    = mkAction (mkVarT "ringTone") e
cupTaken    = mkAction (mkVarT "cupTaken") e

actions :: Var [Action]
actions =  mkVarList [oneEuro, oneDollar, sugar, noSugar, coffee_, tea_, cappuccino_, pourSugar, pourCoffee, pourTea, pourMilk, displayDone, skipRingTone, ringTone, cupTaken]

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

