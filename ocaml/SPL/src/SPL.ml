(* Software Product Lines (SPL) Analysis Framework *)

open List
open Prop

type 'a atom = 'a * presenceCondition

let atom_equal ((v1, pc1) : 'a atom) ((v2, pc2) : 'a atom) : bool =
  (v1 = v2) && (equal pc1 pc2)

type 'a var = ('a atom) list

let rec var_equal (v1:'a var) (v2:'a var) : bool =
  match (v1,v2) with
  | ([], []) -> true
  | (x1::xs1, x2::xs2) -> (atom_equal x1 x2) && var_equal xs1 xs2
  | _ -> false

let mkVar (v: 'a) (p: presenceCondition) : 'a var = [(v,p)]
let mkVarT v = mkVar v tt

let apply (fn : ('a -> 'b) var) (x : 'a var) : 'b var = 
  let allPairs = List.concat (List.map(fun (fn',fnpc') -> List.map (fun (x',xpc') -> (fn' x', conj fnpc' xpc')) x) fn)
  in  filter (fun (x,xpc) -> sat xpc) allPairs

let apply2 (fn: ('a -> 'b -> 'c) var) (x: 'a var) (y: 'b var) : 'c var =
  apply (apply fn x) y
