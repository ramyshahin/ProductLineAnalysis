-- VendingMachine.hs
-- Ramy Shahin
-- March 17th 2017
module VendingMachine where

import Shallow.FTS
import Shallow.VList
import Prop
import SPL
--import Test.HUnit (Test(..), assertEqual, runTestTT)
--import Control.Applicative

-- Features
f = mkUniverse ["coffee", "tea", "cappuccino", "ringTone", "euro", "dollar"]

coffee, tea, cappuccino, ringTone, euro, dollar :: Prop
[coffee, tea, cappuccino, ringTone, euro, dollar] = f

--states :: [State]
states = mkVarStates 0 13
--states = mkVList states

oneEuro, oneDollar, sugar, noSugar, coffee_, tea_, cappuccino_, pourSugar, pourCoffee, pourTea, pourMilk, displayDone, skipRingTone, ringTone_, cupTaken :: VAction
oneEuro      = mkVAction "oneEuro" []
oneDollar    = mkVAction "oneDollar" []
sugar        = mkVAction "sugar" []
noSugar      = mkVAction "noSugar" []
coffee_      = mkVAction "coffee_" []
tea_         = mkVAction "tea_" []
cappuccino_  = mkVAction "cappuccino_" []
pourSugar    = mkVAction "pourSugar" []
pourCoffee   = mkVAction "pourCoffee" []
pourTea      = mkVAction "pourTea" []
pourMilk     = mkVAction "pourMilk" []
displayDone  = mkVAction "displayDone" []
skipRingTone = mkVAction "skipRingTone" []
ringTone_    = mkVAction "ringTone" []
cupTaken     = mkVAction "cupTaken" []

actions :: [VAction]
actions = [oneEuro, oneDollar, sugar, noSugar, coffee_, tea_, cappuccino_, pourSugar, pourCoffee, pourTea, pourMilk, displayDone, skipRingTone, ringTone_, cupTaken]

-- transitions
t1 =  mkTransition (states|!!|0) (states|!!|1)  (mkVList [oneEuro]) euro
t2 =  mkTransition (states|!!|0) (states|!!|1)  (mkVList [oneDollar]) dollar
t3 =  mkTransition (states|!!|1) (states|!!|2)  (mkVList [sugar]) tt
t4 =  mkTransition (states|!!|1) (states|!!|3)  (mkVList [noSugar]) tt
t5 =  mkTransition (states|!!|2) (states|!!|4)  (mkVList [coffee_]) coffee
t6 =  mkTransition (states|!!|2) (states|!!|5)  (mkVList [tea_]) tea
t7 =  mkTransition (states|!!|2) (states|!!|6)  (mkVList [cappuccino_]) cappuccino
t8 =  mkTransition (states|!!|3) (states|!!|7)  (mkVList [cappuccino_]) cappuccino
t9 =  mkTransition (states|!!|3) (states|!!|8)  (mkVList [tea_]) tea
t10 =  mkTransition (states|!!|3) (states|!!|9)  (mkVList [coffee_]) coffee
t11 =  mkTransition (states|!!|4) (states|!!|9)  (mkVList [pourSugar]) coffee
t12 =  mkTransition (states|!!|5) (states|!!|8)  (mkVList [pourSugar]) tea
t13 =  mkTransition (states|!!|6) (states|!!|7)  (mkVList [pourSugar]) cappuccino
t14 =  mkTransition (states|!!|7) (states|!!|10)  (mkVList [pourCoffee]) cappuccino
t15 =  mkTransition (states|!!|8) (states|!!|11)  (mkVList [pourTea]) tea
t16 =  mkTransition (states|!!|9) (states|!!|11)  (mkVList [pourCoffee]) coffee
t17 =  mkTransition (states|!!|10) (states|!!|11)  (mkVList [pourMilk]) cappuccino
t18 =  mkTransition (states|!!|11) (states|!!|13)  (mkVList [displayDone]) tt
t19 =  mkTransition (states|!!|13) (states|!!|12)  (mkVList [skipRingTone]) (neg ringTone)
t20 =  mkTransition (states|!!|13) (states|!!|12)  (mkVList [ringTone_]) ringTone
t21 =  mkTransition (states|!!|12) (states|!!|0)  (mkVList [cupTaken]) tt

transitions = mkVList [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10,
               t11, t12, t13, t14, t15, t16, t17, t18, t19, t20,
               t21]
            
vendingMachine = mkFTS states (mkVList actions) transitions (mkVList [states|!!|0])
