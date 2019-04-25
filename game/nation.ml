type kind = Clan | Hekatium | Numendor | Sodistan | Tulron
type support = (kind * Resource.t) list
type trade = Boost of kind | Certain of kind | NoTrade
type t = { chosen : kind list; trade : trade; trading : bool }

let empty = { chosen = []; trade = NoTrade; trading = false }
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

let trade t = t.trade
let which t = t.chosen

let boost kind t =
  { t with trade = Boost kind }

let certain kind t =
  { t with trade = Certain kind }

let chosen ls t =
  { t with chosen =
    Listx.in_both ls kinds
    |> Listx.pick_first max_allowed
  }

let trading trading t =
  { t with trading }

module Roll (Dice : Dice.S) = struct
  let support t =
    let roll (a, b) = Dice.between a b in
    let to_res kind =
      let (a, b) = ranges_of kind in
      let m = roll a in
      let s = roll b in
      let s' = if t.trading && t.trade = Boost kind then 10 else 0 in
      Resource.(of_manp m <+ Supply (s + s'))
    in
    let chance kind =
      if t.trading && t.trade = Certain kind || Dice.chance 0.8
      then to_res kind
      else Resource.empty
    in
    List.map (fun kind -> kind, chance kind) t.chosen
end
