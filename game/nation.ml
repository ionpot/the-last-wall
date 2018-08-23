type t = Tulron | Sodistan | Hekatium | Numendor | Clan
type support = (t * Resource.t option)

let max_allowed = 3

let t_list =
  [Tulron; Sodistan; Hekatium; Numendor; Clan]

let ranges_of =
  let low = (0, 10) in
  let mid = (10, 20) in
  let high = (20, 30) in
  let f x = Pair.(x <+> 5) in
  function
    | Tulron
    | Sodistan -> (high, f low)
    | Hekatium
    | Numendor -> (low, f high)
    | Clan -> (mid, f mid)

let roll (a, b) =
  Dice.between a b

let filter nats =
  let f n = List.mem n nats in
  t_list
  |> List.filter f
  |> Listx.pick_first max_allowed

let total_of ns =
  let open Resource in
  let f total (_, maybe) =
    match maybe with
    | Some res -> total ++ res
    | None -> total
  in
  List.fold_left f empty ns

let to_outcome nat =
  let f () =
    let (a, b) = ranges_of nat in
    let m = roll a in
    let s = roll b in
    Resource.(of_manp m <+ Supply s)
  in
  if Dice.chance 0.8
  then Some (f ())
  else None

let support_of n =
  (n, to_outcome n)

let support_of_list ns =
  List.map support_of ns

let apply_bonus ls res =
  let f = function
    | (n, Some x) -> (n, Some Resource.(x ++ res))
    | x -> x
  in
  List.map f ls
