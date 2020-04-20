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
  type t = Defs.percent Map.t
  let cap = 80
  let cap_trading = 90
  let empty : t = Map.empty
  let of_kind = Map.find
  let map f k t = Map.add k (of_kind k t |> f) t
  let add step = map (Number.add_if_ptv step)
  let cap_at cap = map (min cap)
  let set_trading k = Map.add k cap_trading
  let sub step = map (Number.sub_by step)
  let sub_all step = Map.map (Number.sub_by step)
end

type t =
  { barracks : kind option
  ; chances : Chance.t
  ; chosen : Set.t
  ; support : support
  ; trade : kind option
  }

let empty =
  { barracks = None
  ; chances = Chance.empty
  ; chosen = Set.empty
  ; support = Map.empty
  ; trade = None
  }

let barracks t = t.barracks
let chances t = t.chances
let chosen t = t.chosen
let trade t = t.trade

let has_aided k t =
  Map.mem k t.support

let has_barracks k t =
  t.barracks = Some k

let has_trade k t =
  t.trade = Some k

let has_traded kind t =
  has_trade kind t && has_aided kind t

let no_barracks t =
  t.barracks = None

let no_trade t =
  t.trade = None

let cap_of kind t =
  if has_trade kind t
  then Chance.cap_trading
  else Chance.cap

let chances_init t =
  let f m k = Map.add k (cap_of k t) m in
  List.fold_left f Chance.empty kinds

let mnp_from k t =
  if has_aided k t
  then Map.find k t.support |> Resource.mnp
  else 0

let sup_from k t =
  if has_aided k t
  then Map.find k t.support |> Resource.sup
  else 0

let traded_mnp kind t =
  has_trade kind t
  |> Number.if_ok (mnp_from kind t)

let traded_sup kind t =
  has_trade kind t
  |> Number.if_ok (sup_from kind t)

let set_chosen chosen t =
  { t with chosen }

let set_barracks barracks t =
  { t with barracks }

let set_chances chances t =
  { t with chances }

let map_chances f t =
  { t with chances = f t.chances }

let set_support m t =
  { t with support = Map.filter (fun _ -> (<>) Resource.empty) m }

let set_trade trade t =
  { t with trade }

let trim_chances t =
  map_chances (Map.mapi (fun k v -> min v (cap_of k t))) t
