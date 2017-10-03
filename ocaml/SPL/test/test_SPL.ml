open OUnit2
open SPL
open Prop

(* presence conditions *)
let p_a = atom 1
let p_b = atom 2
let n_a = neg p_a
let n_b = neg p_b
let p_a_and_p_b = conj p_a p_b
let p_a_and_n_b = conj p_a n_b
let n_a_and_p_b = conj n_a p_b
let n_a_and_n_b = conj n_a n_b

(* test objects *)
let u = [(18, p_a_and_p_b); (12, p_a_and_n_b); (0, n_a_and_p_b); (3, n_a_and_n_b)]
let v = [(-5, p_a_and_p_b); (9, p_a_and_n_b); (2, n_a_and_p_b); (8, n_a_and_n_b)]
let w = [(1, p_a_and_p_b); (6, p_a_and_n_b); (10, n_a)]
let x = [(7,p_a); (11, n_a)]
let y = [(13, p_a_and_p_b); (-23, n_a_and_p_b); (20, n_b)]

let liftedNeg = mkVarT (~-)
let liftedNeg_tst1 ctxt = assert_bool "" (var_equal [(-7,p_a); (-11, n_a)] (apply liftedNeg x))
let liftedNeg_tst2 ctxt = assert_bool "" (var_equal [(-13, p_a_and_p_b); (23, n_a_and_p_b); (-20, n_b)] (apply liftedNeg y))

let liftedPlus = mkVarT (+)
let liftedPlus_tst1 ctxt = assert_bool "" (var_equal [(13, p_a_and_p_b); (21, p_a_and_n_b); (2, n_a_and_p_b); (11, n_a_and_n_b)] (apply2 liftedPlus u v))

let suite =
  "suite">:::
   ["liftedNeg_tst1">:: liftedNeg_tst1;
    "liftedNeg_tst2">:: liftedNeg_tst2;
    "liftedPlus_tst1">:: liftedPlus_tst1
   ]

let _ = run_test_tt_main suite 