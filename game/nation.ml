type kind = Clan | Hekatium | Numendor | Sodistan | Tulron
type trade = Boost of kind | Certain of kind | NoTrade

module Map = Map.Make(struct
  type t = kind
  let compare = compare
end)

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

module Chance = struct
  type t = float Map.t
  let base = 0.8
  let base_map : t =
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
  { chances : Chance.t;
    chosen : kind list
  }

let empty =
  { chances = Chance.base_map;
    chosen = []
  }

let chances t = t.chances
let which t = t.chosen

let chosen ls t =
  { t with chosen = Listx.pick_first max_allowed ls }

let map_chances f t =
  { t with chances = f t.chances }
