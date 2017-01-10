import LTS

type State = String
type Guard = String
type Action = String

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
heaterOn    = "heaterOn"
heaterOff   = "heaterOff"
washStart   = "washStart"
tempCheck   = "tempCheck"
setDelay    = "setDelay"
quickCool   = "quickCool"

actions :: [Action]
actions =  [heaterOn, heaterOff, washStart, tempCheck, setDelay, quickCool]

heatEnabled :: Guard
heatEnabled = "heatEnabled"

guards :: [Guard]
guards = [heatEnabled]

transitions :: [Transition State Guard Action]
--                          from        to          guards          actions
transitions =  [(Transition start       locking     []              []),
                (Transition locking     waiting     [heatEnabled]   [heaterOn]),
                (Transition waiting     washing     []              [heaterOff, washStart]),
                (Transition washing     unlocking   []              [quickCool]),
                (Transition washing     drying      []              []),
                (Transition drying      unlocking   []              [quickCool]),
                (Transition unlocking   finish      []              [])
               ]

washingMachine :: LTS State Guard Action
washingMachine = LTS states guards actions transitions [start]
