(* Software Product Lines (SPL) Analysis Framework

module SPL*)

open List

type presenceCondition = int

let conj pc1 pc2 = pc1 + pc2

let sat pc = pc != 0

type 'a atom = 'a * presenceCondition

type 'a var = ('a atom) list

let mkVar v p = [(v,p)]

let apply fn x = 
  let allPairs = List.concat (List.map(fun (fn',fnpc') -> List.map (fun (x',xpc') -> (fn' x', conj fnpc' xpc')) x) fn)
  in  filter (fun (x,xpc) -> sat xpc) allPairs

let a = mkVar 'a' 1
let b = mkVar 'b' 2
let c = mkVar 'c' 3