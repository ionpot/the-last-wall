type kind = Tulron | Sodistan | Hekatium | Numendor | Clan
type support = (kind * Resource.t)
type t = kind list

let empty = []
let kinds = [Tulron; Sodistan; Hekatium; Numendor; Clan]
let max_allowed = 3

let ranges_of =
  let low = (0, 10) in
  let mid = (10, 20) in
  let high = (20, 30) in
  let f x = Pair.(x <+> 10) in
  function
    | Tulron
    | Sodistan -> (high, f low)
    | Hekatium
    | Numendor -> (low, f high)
    | Clan -> (mid, f mid)

let from ls =
  Listx.in_both ls kinds
  |> Listx.pick_first max_allowed

let add res ls =
  let f (kind, res') =
    kind, Resource.(res ++ res')
  in
  List.map f ls

let sum ls =
  let f total (_, res) =
    Resource.(total ++ res)
  in
  List.fold_left f Resource.empty ls

let support t =
  let roll (a, b) = Dice.between a b in
  let to_res kind =
    let (a, b) = ranges_of kind in
    let m = roll a in
    let s = roll b in
    Resource.(of_manp m <+ Supply s)
  in
  let chance kind =
    if Dice.chance 0.8
    then to_res kind
    else Resource.empty
  in
  List.map (fun kind -> kind, chance kind) t

let which t = t
