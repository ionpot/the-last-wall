type kind = Clan | Hekatium | Numendor | Sodistan | Tulron
type support = (kind * Resource.t) list
type trade = Boost of kind | Certain of kind | NoTrade
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

let which t = t

let chosen ls t =
  Listx.pick_first max_allowed t

module Roll (Dice : Dice.S) = struct
  let roll (a, b) = Dice.between a b

  let roll_res kind trade =
    let (a, b) = ranges_of kind in
    let m = roll a in
    let s = roll b in
    let s' = if trade = Boost kind then 10 else 0 in
    Resource.(of_manp m <+ Supply (s + s'))

  let to_res kind trade =
    if trade = Certain kind || Dice.chance 0.8
    then roll_res kind trade
    else Resource.empty

  let support trade t =
    List.map (fun kind -> kind, to_res kind trade) t
end
