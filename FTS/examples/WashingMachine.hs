import LTS
import Test.HUnit (Test(..), assertEqual, runTestTT)

start, locking, waiting, washing, drying, unlocking, finish :: State
start       = "start"
locking     = "locking"
waiting     = "waiting"
washing     = "washing"
drying      = "drying"
unlocking   = "unlocking"
finish      = "finish"

states :: [State]
states = [start, locking, waiting, washing, drying, unlocking, finish]

heaterOn, heaterOff, washStart, tempCheck, setDelay, quickCool :: Action
heaterOn    = Action "heaterOn" []
heaterOff   = Action "heaterOff" []
washStart   = Action "washStart" []
tempCheck   = Action "tempCheck" []
setDelay    = Action "setDelay"  []
quickCool   = Action "quickCool" []

actions :: [Action]
actions =  [heaterOn, heaterOff, washStart, tempCheck, setDelay, quickCool]

heatEnabled :: Guard
heatEnabled = "heatEnabled"

transitions :: [Transition]
--                          from        to          actions
transitions =  [(Transition start       locking     []),
                (Transition locking     waiting     [heaterOn]),
                (Transition waiting     washing     [heaterOff, washStart]),
                (Transition washing     unlocking   [quickCool]),
                (Transition washing     drying      []),
                (Transition drying      unlocking   [quickCool]),
                (Transition unlocking   finish      [])
               ]

washingMachine :: LTS
washingMachine = LTS states actions transitions [start]

-- neighborsTests
neighborsStart      = TestCase (assertEqual "neighborsStart"        (neighbors transitions start)       [locking])
neighborsLocking    = TestCase (assertEqual "neighborsLocking"      (neighbors transitions locking)     [waiting])
neighborsWaiting    = TestCase (assertEqual "neighborsWaiting"      (neighbors transitions waiting)     [washing])
neighborsWashing    = TestCase (assertEqual "neighborsWashing"      (neighbors transitions washing)     [unlocking, drying])
neighborsDrying     = TestCase (assertEqual "neighborsDrying"       (neighbors transitions drying)      [unlocking])
neighborsUnlocking  = TestCase (assertEqual "neighborsUnlocking"    (neighbors transitions unlocking)   [finish])
neighborsFinish     = TestCase (assertEqual "neighborsFinish"       (neighbors transitions finish)      [])

neighborsTests = TestList [neighborsStart, neighborsLocking, neighborsWaiting, neighborsWashing, 
                  neighborsDrying, neighborsUnlocking, neighborsFinish]

main :: IO ()
main = do
    putStrLn "TODO"
