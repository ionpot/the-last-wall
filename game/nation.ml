type kind = Clan | Hekatium | Numendor | Sodistan | Tulron

module Kind = struct
  type t = kind
  let compare = compare
end

module Map = Map.Make(Kind)
module Set = Set.Make(Kind)

type support = Resource.t Map.t

let kinds = [Tulron; Sodistan; Hekatium; Numendor; Clan]
let max_allowed = 3

let ranges_of =
  let low = (0, 10) in
  let mid = (10, 20) in
  let high = (20, 30) in
  let f = Range.Int.add 10 in
  function
    | Tulron
    | Sodistan -> (high, f low)
    | Hekatium
    | Numendor -> (low, f high)
    | Clan -> (mid, f mid)

let set2map f set =
  let g kind map = Map.add kind (f kind) map in
  Set.fold g set Map.empty

module Chance = struct
  type t = Defs.chance Map.t
  let cap = 0.8
  let cap_trading = 0.9
  let base_map : t =
    let f m k = Map.add k cap m in
    List.fold_left f Map.empty kinds
  let of_kind = Map.find
  let map f k t = Map.add k (of_kind k t |> f) t
  let add step = map (Float.add_if_ptv step)
  let cap_at cap = map (min cap)
  let set_trading k = Map.add k cap_trading
  let sub step = map (Float.sub_by step)
  let sub_all step = Map.map (Float.sub_by step)
end

type t =
  { chances : Chance.t
  ; chosen : Set.t
  ; support : support
  }

let empty =
  { chances = Chance.base_map
  ; chosen = Set.empty
  ; support = Map.empty
  }

let chances t = t.chances
let chosen t = t.chosen

let has_aided k t =
  Map.mem k t.support

let mnp_from k t =
  if has_aided k t
  then Map.find k t.support |> Resource.manp_of
  else 0

let sup_from k t =
  if has_aided k t
  then Map.find k t.support |> Resource.supp_of
  else 0

let set_chosen chosen t =
  { t with chosen }

let map_chances f t =
  { t with chances = f t.chances }

let set_support m t =
  { t with support = Map.filter (fun _ -> (<>) Resource.empty) m }
