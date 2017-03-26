-- VendingMachine.hs
-- Ramy Shahin
-- March 17th 2017
module VendingMachine where

import Deep.FTS
import Prop
import SPL
--import Test.HUnit (Test(..), assertEqual, runTestTT)
--import Control.Applicative

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

--states :: [State]
states' = mkStates 0 13
states = mkVList states'

oneEuro, oneDollar, sugar, noSugar, coffee_, tea_, cappuccino_, pourSugar, pourCoffee, pourTea, pourMilk, displayDone, skipRingTone, ringTone_, cupTaken :: Action
oneEuro      = Action "oneEuro" []
oneDollar    = Action "oneDollar" []
sugar        = Action "sugar" []
noSugar      = Action "noSugar" []
coffee_      = Action "coffee_" []
tea_         = Action "tea_" []
cappuccino_  = Action "cappuccino_" []
pourSugar    = Action "pourSugar" []
pourCoffee   = Action "pourCoffee" []
pourTea      = Action "pourTea" []
pourMilk     = Action "pourMilk" []
displayDone  = Action "displayDone" []
skipRingTone = Action "skipRingTone" []
ringTone_    = Action "ringTone" []
cupTaken     = Action "cupTaken" []

actions :: [Action]
actions = [oneEuro, oneDollar, sugar, noSugar, coffee_, tea_, cappuccino_, pourSugar, pourCoffee, pourTea, pourMilk, displayDone, skipRingTone, ringTone_, cupTaken]

-- transitions
t1 =  mkTransition (states'!!0) (states'!!1)  (mkVarT [oneEuro]) euro
t2 =  mkTransition (states'!!0) (states'!!1)  (mkVarT [oneDollar]) dollar
t3 =  mkTransition (states'!!1) (states'!!2)  (mkVarT [sugar]) T
t4 =  mkTransition (states'!!1) (states'!!3)  (mkVarT [noSugar]) T
t5 =  mkTransition (states'!!2) (states'!!4)  (mkVarT [coffee_]) coffee
t6 =  mkTransition (states'!!2) (states'!!5)  (mkVarT [tea_]) tea
t7 =  mkTransition (states'!!2) (states'!!6)  (mkVarT [cappuccino_]) cappuccino
t8 =  mkTransition (states'!!3) (states'!!7)  (mkVarT [cappuccino_]) cappuccino
t9 =  mkTransition (states'!!3) (states'!!8)  (mkVarT [tea_]) tea
t10 =  mkTransition (states'!!3) (states'!!9)  (mkVarT [coffee_]) coffee
t11 =  mkTransition (states'!!4) (states'!!9)  (mkVarT [pourSugar]) coffee
t12 =  mkTransition (states'!!5) (states'!!8)  (mkVarT [pourSugar]) tea
t13 =  mkTransition (states'!!6) (states'!!7)  (mkVarT [pourSugar]) cappuccino
t14 =  mkTransition (states'!!7) (states'!!10)  (mkVarT [pourCoffee]) cappuccino
t15 =  mkTransition (states'!!8) (states'!!11)  (mkVarT [pourTea]) tea
t16 =  mkTransition (states'!!9) (states'!!11)  (mkVarT [pourCoffee]) coffee
t17 =  mkTransition (states'!!10) (states'!!11)  (mkVarT [pourMilk]) cappuccino
t18 =  mkTransition (states'!!11) (states'!!13)  (mkVarT [displayDone]) T
t19 =  mkTransition (states'!!13) (states'!!12)  (mkVarT [skipRingTone]) (neg ringTone)
t20 =  mkTransition (states'!!13) (states'!!12)  (mkVarT [ringTone_]) ringTone
t21 =  mkTransition (states'!!12) (states'!!0)  (mkVarT [cupTaken]) T

transitions = mkVList []--t1]--, t2]--, t3, t4, t5, t6, t7, t8, t9, t10,
               --t11, t12, t13, t14, t15, t16, t17, t18, t19, t20,
               --t21]
            
vendingMachine = mkFTS states (mkVarT actions) transitions (mkVList [states'!!0]) T
