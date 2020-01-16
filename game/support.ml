module Chance = Nation.Chance
module Map = Nation.Map

type chances = Chance.t
type t = Nation.support

let is_empty k t =
  Map.mem k t && Map.find k t = Resource.empty

let sum t =
  let f _ = Resource.(++) in
  Map.fold f t Resource.empty

module Check (S : State.S) = struct
  let winter = S.Month.check Month.is_winter
  let has_barracks kind =
    S.Nation.check Nation.(has_barracks kind)
  let has_trade kind =
    S.Nation.check Nation.(has_trade kind)
  let cap_of kind =
    if has_trade kind
    then Chance.cap_trading
    else Chance.cap
  let has_traded kind =
    has_trade kind && S.Nation.check (Nation.has_aided kind)
  let traded_mnp kind =
    let mnp = S.Nation.return (Nation.mnp_from kind) in
    has_trade kind |> Number.if_ok mnp
  let traded_sup kind =
    let sup = S.Nation.return (Nation.sup_from kind) in
    has_trade kind |> Number.if_ok sup
end

module Apply (S : State.S) = struct
  module Check = Check(S)

  let adjust kind t cmap =
    (if is_empty kind t
    then Chance.sub
    else Chance.add) 10 kind cmap

  let boost kind cmap =
    if Check.has_trade kind
    then Chance.add 5 kind cmap
    else cmap

  let cap kind =
    Chance.cap_at (Check.cap_of kind) kind

  let chances t cmap =
    let f cmap kind =
      adjust kind t cmap
      |> boost kind
      |> cap kind
    in
    List.fold_left f cmap Nation.kinds

  let set_bonus b nat =
    let yes = Check.has_traded nat in
    S.Bonus.map Bonus.(set b yes)

  let value t =
    chances t |> Nation.map_chances |> S.Nation.map;
    Nation.set_support t |> S.Nation.map;
    set_bonus Bonus.ClanTrade Nation.Clan;
    set_bonus Bonus.NumendorTrade Nation.Numendor
end

module Roll (S : State.S) = struct
  module Check = Check(S)

  let bonuses kind res =
    let barracks = Check.has_barracks kind in
    let trade = Check.has_trade kind in
    let hekatium = kind = Nation.Hekatium in
    let mnp = Number.if_ok 10 barracks in
    let sup = Number.if_ok 10 trade in
    Resource.add ~mnp ~sup res
    |> Resource.(bonus_if (trade && hekatium) Bonus.(Add (Both 0.1)))

  let roll = S.Dice.range

  let chance_of kind nats =
    Nation.chances nats
    |> Chance.of_kind kind
    |> Number.sub_if Check.winter 10

  let roll_res kind =
    let (a, b) = Nation.ranges_of kind in
    Resource.make ~mnp:(roll a) ~sup:(roll b) ()

  let to_res nats kind =
    if chance_of kind nats |> S.Dice.percent
    then roll_res kind |> bonuses kind
    else Resource.empty

  let from nats =
    Nation.(chosen nats |> set2map (to_res nats))
end
