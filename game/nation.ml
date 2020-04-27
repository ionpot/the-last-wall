type kind = Clan | Hekatium | Numendor | Sodistan | Tulron

module Kind = struct
  type t = kind
  let compare = compare
end

module Map = Map.Make(Kind)
module Mapx = Mapx.Make(Map)
module Set = Set.Make(Kind)

type chances = Defs.percent Map.t
type resources = Resource.t Map.t

let kinds = [Tulron; Sodistan; Hekatium; Numendor; Clan]
let max_allowed = 3

type t =
  { barracks : kind option
  ; chances : chances
  ; chosen : Set.t
  ; support : resources
  ; trade : kind option
  }

let empty =
  { barracks = None
  ; chances = Map.empty
  ; chosen = Set.empty
  ; support = Mapx.of_list kinds Resource.empty
  ; trade = None
  }

let barracks t = t.barracks
let chances t = t.chances
let chosen t = t.chosen
let support t = t.support
let trade t = t.trade

let has_aided k t =
  Map.find k t.support <> Resource.empty

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

let mnp_from k t =
  Map.find k t.support |> Resource.mnp

let sup_from k t =
  Map.find k t.support |> Resource.sup

let traded_mnp kind t =
  has_trade kind t
  |> Number.if_ok (mnp_from kind t)

let traded_sup kind t =
  has_trade kind t
  |> Number.if_ok (sup_from kind t)

let barracks_set barracks t =
  { t with barracks }

let chances_map f t =
  { t with chances = f t.chances }

let chances_set chances t =
  { t with chances }

let chosen_set chosen t =
  { t with chosen }

let support_set support t =
  { t with support }

let trade_set trade t =
  { t with trade }
