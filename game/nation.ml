type t = Tulron | Sodistan | Hekatium | Numendor | Clan
type resource = Resource.t
type support = (t * resource option)

let t_list =
  [Tulron; Sodistan; Hekatium; Numendor; Clan]

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

let to_outcome () =
  let f () =
    let a = Dice.between 5 30 in
    let b = Dice.between 5 20 in
    let (m, s) =
      if Random.bool ()
      then (a, b)
      else (b, a)
    in
    Resource.(make (Manpwr m) <+ Supply s)
  in
  if Dice.chance 0.8
  then Some (f ())
  else None

let support_of n =
  (n, to_outcome ())

let support_of_list ns =
  List.map support_of ns
