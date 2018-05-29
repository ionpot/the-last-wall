type t = Tulron | Sodistan | Hekatium | Numendor | Clan
type resource = Resource.t
type support = (t * resource option)

let t_list =
  [Tulron; Sodistan; Hekatium; Numendor; Clan]

let ranges_of =
  let low = (0, 10) in
  let mid = (10, 20) in
  let high = (20, 30) in
  function
    | Tulron
    | Sodistan -> (high, low)
    | Hekatium
    | Numendor -> (low, high)
    | Clan -> (mid, mid)

let roll (a, b) =
  Dice.between a b

let pickN max nats =
  let f n = List.mem n nats in
  t_list
  |> List.filter f
  |> Listx.pick_first max

let total_of ns =
  let open Resource in
  let f total (_, maybe) =
    match maybe with
    | Some res -> total ++ res
    | None -> total
  in
  List.fold_left f (make Empty) ns

let to_outcome nat =
  let f () =
    let (a, b) = ranges_of nat in
    let m = roll a in
    let s = roll b in
    Resource.(make (Manpwr m) <+ Supply s)
  in
  if Dice.chance 0.8
  then Some (f ())
  else None

let support_of n =
  (n, to_outcome n)

let support_of_list ns =
  List.map support_of ns
