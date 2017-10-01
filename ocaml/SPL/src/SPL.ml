(* Software Product Lines (SPL) Analysis Framework

module SPL*)

open List
open Prop

type 'a atom = 'a * presenceCondition

type 'a var = ('a atom) list

let mkVar (v: 'a) (p: presenceCondition) : 'a var = [(v,p)]
let mkVarT v = mkVar v tt

let apply (fn : ('a -> 'b) var) (x : 'a var) : 'b var = 
  let allPairs = List.concat (List.map(fun (fn',fnpc') -> List.map (fun (x',xpc') -> (fn' x', conj fnpc' xpc')) x) fn)
  in  filter (fun (x,xpc) -> sat xpc) allPairs

let liftedNeg = mkVarT (~-)

(* presence conditions *)
let p_a = atom 1
let p_b = atom 2
let n_a = neg p_a
let n_b = neg p_b
let p_a_and_p_b = conj p_a p_b
let p_a_and_n_b = conj p_a n_b
let n_a_and_p_b = conj n_a p_b
let n_a_and_n_b = conj n_a n_b

let u = [(18, p_a_and_p_b); (12, p_a_and_n_b); (0, n_a_and_p_b); (3, n_a_and_n_b)]
let v = [(-5, p_a_and_p_b); (9, p_a_and_n_b); (2, n_a_and_p_b); (8, n_a_and_n_b)]
let w = [(1, p_a_and_p_b); (6, p_a_and_n_b); (10, n_a)]
let x = [(7,p_a); (11, n_a)]
let y = [(13, p_a_and_p_b); (-23, n_a_and_p_b); (20, n_b)]
