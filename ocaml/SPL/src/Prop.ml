open MLBDD

let mgr = MLBDD.init ~cache:100 ()

type presenceCondition = MLBDD.t

let compare (a:presenceCondition) (b:presenceCondition) : int = if MLBDD.equal a b then 0 else -1
let equal (a:presenceCondition) (b:presenceCondition) : bool = MLBDD.equal a b

let tt = dtrue mgr
let ff = dfalse mgr

let atom id = ithvar mgr id
let neg = dnot
let conj pc1 pc2 = dand pc1 pc2
let disj pc1 pc2 = dor pc1 pc2
let impl pc1 pc2 = imply pc1 pc2

let unsat = is_false

let sat = (fun x -> (not (unsat x)))



