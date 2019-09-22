type kind = Clan | Hekatium | Numendor | Sodistan | Tulron
type support = (kind * Resource.t) list
type trade = Boost of kind | Certain of kind | NoTrade

module Map = Map.Make(struct
  type t = kind
  let compare = compare
end)

let kinds = [Tulron; Sodistan; Hekatium; Numendor; Clan]
let max_allowed = 3

module Chance = struct
  type map = float Map.t
  let base = 0.8
  let base_map : map =
    let f m k = Map.add k base m in
    List.fold_left f Map.empty kinds
  let of_kind = Map.find
  let deduct k map =
    let c = of_kind k map -. 0.1 in
    Map.add k c map
  let reset k map =
    Map.add k base map
end

type t =
  { chances : Chance.map;
    chosen : kind list
  }

let empty =
  { chances = Chance.base_map;
    chosen = []
  }

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
  let try_add a b =
    if b = Resource.empty then b else Resource.(a ++ b)
  in
  List.map (fun (k, r) -> k, try_add res r) ls

let sum ls =
  let f total (_, res) =
    Resource.(total ++ res)
  in
  List.fold_left f Resource.empty ls

let which t = t.chosen

let chosen ls t =
  { t with chosen = Listx.pick_first max_allowed ls }

let update_chances ls t =
  let f cmap (kind, res) =
    if res = Resource.empty
    then Chance.deduct kind cmap
    else Chance.reset kind cmap
  in
  { t with chances = List.fold_left f t.chances ls }

module Roll (Dice : Dice.S) = struct
  let roll (a, b) = Dice.between a b

  let roll_chance kind chances =
    Chance.of_kind kind chances |> Dice.chance

  let roll_res kind trade =
    let (a, b) = ranges_of kind in
    let m = roll a in
    let s = roll b in
    let s' = if trade = Boost kind then 10 else 0 in
    Resource.(of_manp m <+ Supply (s + s'))

  let to_res kind trade chances =
    if trade = Certain kind || roll_chance kind chances
    then roll_res kind trade
    else Resource.empty

  let support trade t =
    List.map (fun kind -> kind, to_res kind trade t.chances) t.chosen
end
