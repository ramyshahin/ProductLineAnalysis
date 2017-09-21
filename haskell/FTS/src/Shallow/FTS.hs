-------------------------------------------------------------------------------
-- LiftedLTS.hs
-- Lifted Labeled Transition System (LTS) library
-- Ramy Shahin - July 20th 2016
-------------------------------------------------------------------------------
module Shallow.FTS where
import LTS
import Data.List
import Shallow.VList
import Control.Applicative
import SPL
import Debug.Trace
import Control.Exception

mkVarStates :: Int -> Int -> VList State
mkVarStates begin end =
    if begin > end then vNil else vCons (mkVarT s) (mkVarStates (begin + 1) end)
    where s = "s" ++ (show begin)

type VState = Var State
type VGuard = Var Guard
type VAction = Var Action

mkVAction s gs = (liftV2 Action) (mkVarT s) (mkVarT gs)

type VTransition = Var Transition

mkTransition src tgt actions pc = restrict pc ((liftV3 Transition) src tgt actions)
vsource = liftV source
vtarget = liftV target
vact = liftV act

type FTS = Var LTS

mkFTS = liftV4 LTS

vneighbors :: Var [Transition] -> VState -> Var [State]

vneighbors = liftV2 neighbors

visReachable = liftV2 isReachable

vwitnessPath = liftV2 witnessPath

vdfs = liftV4 dfs

