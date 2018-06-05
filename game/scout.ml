type t = Enemy.t
type count = Enemy.count
type enemies = Enemy.party list
type report = (count * t) list
type sum_report = (count * t list)

let cost = Resource.(make (Supply 10))

let round x =
  let a = 0.1 *. float x in
  let f = if Random.bool () then floor else ceil in
  10 * truncate (f a)

let try_round x =
  if x > 10 then round x else x

let report_of enemies =
  let f e =
    Enemy.((count_of e |> try_round, type_of e))
  in
  List.map f enemies

let sum_report_of enemies =
  let f a e = a + Enemy.count_of e in
  let total = List.fold_left f 0 enemies in
  let seen = List.map (fun e -> Enemy.type_of e) enemies in
  (try_round total, seen)
