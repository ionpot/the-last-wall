module B = Building
module R = Resource

type t = (B.t * R.t) list

let empty = []

let add b t =
  (b, B.cost_of b) :: t

let sub_manp m cost =
  if R.has_supp cost
  then (m, cost)
  else R.deduce_manp m cost

let apply_manp m t =
  let f (b, cost1) (m1, ls) =
    let m2, cost2 = sub_manp m1 cost1 in
    m2, (b, cost2) :: ls
  in
  List.fold_right f t (m, [])
  |> snd

let deduce supply t =
  let f (b, cost1) (s1, ls) =
    let s2, cost2 = R.deduce_supp s1 cost1 in
    s2, (b, cost2) :: ls
  in
  List.fold_right f t (supply, [])

let status_of t = t

let is_built (_, cost) = cost = R.empty

let tick t =
  let built, needs = List.partition is_built t in
  List.map fst built, needs
