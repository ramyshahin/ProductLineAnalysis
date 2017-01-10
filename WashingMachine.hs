import LTS

import Data.Vector

states :: Vector String
states = fromList ["Start", "Locking", "Waiting", "Washing", "Drying", "Unlocking", "Finish"]

actions :: Vector String
actions = fromList ["HeaterOn", "HeaterOff", "wash.Start", "TempCheck", "SetDelay", "QuickCool"]

transitions :: Vector Transition
transitions = fromList [(Transition 0 1 []),    -- Start    --->            Locking
                        (Transition 1 2 [0]),   -- Locking  -[HeaterOn]->   Waiting
                        (Transition 2 3 [1,2])  -- Waiting  -[HeaterOff, wash.Start]-> Washing
                        ]