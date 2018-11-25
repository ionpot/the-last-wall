module B = Building
module R = Resource

type t = (B.t * R.t) list

let empty = []

let add b t =
  (b, B.cost_of b) :: t

let manp_cost t =
  let f acc (_, res) = acc + R.manp_of res in
  List.fold_left f 0 t

let supp_cost t =
  let f acc (_, res) = acc + R.supp_of res in
  List.fold_left f 0 t

let set_manp m t =
  let module M = (val Value.of_num m) in
  let f (b, res) =
    b, if R.has_supp res then res else R.map_manp M.take res
  in
  List.map f t

let set_supp s t =
  let module S = (val Value.of_num s) in
  let f (b, res) =
    b, R.map_supp S.take res
  in
  List.map f t

let status_of t = t

let is_built (_, cost) = cost = R.empty

let tick t =
  let built, needs = List.partition is_built t in
  List.map fst built, needs
