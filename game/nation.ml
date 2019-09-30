type kind = Clan | Hekatium | Numendor | Sodistan | Tulron

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
  let step = 0.1
  let base_map : t =
    let f m k = Map.add k base m in
    List.fold_left f Map.empty kinds
  let of_kind = Map.find
  let map f k t = Map.add k (of_kind k t |> f) t
  let increase = map (fun c -> Float.add_if_ptv step c |> min base)
  let reduce = map (Float.sub_by step)
end

type t =
  { chances : Chance.t
  ; chosen : kind list
  }

let empty =
  { chances = Chance.base_map
  ; chosen = []
  }

let chances t = t.chances
let which t = t.chosen

let chosen ls t =
  { t with chosen = Listx.pick_first max_allowed ls }

let map_chances f t =
  { t with chances = f t.chances }
