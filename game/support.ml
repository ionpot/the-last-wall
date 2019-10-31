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
  let has_trade kind =
    S.Build.check Build.(has_trade kind)
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
    else Chance.add) 0.1 kind cmap

  let boost kind cmap =
    if Check.has_trade kind
    then Chance.add 0.05 kind cmap
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

  let value t =
    chances t |> Nation.map_chances |> S.Nation.map;
    Nation.set_support t |> S.Nation.map
end

module Roll (S : State.S) = struct
  module Check = Check(S)

  let bonuses kind res =
    let trade = Check.has_trade kind in
    let hekatium = kind = Nation.Hekatium in
    let sup = Number.if_ok 10 trade in
    Resource.bonus_if
      (trade && hekatium)
      Resource.Bonus.(Add (Both 0.1))
      Resource.(res ++ of_supp sup)

  let roll = S.Dice.range

  let chance_of kind nats =
    Nation.chances nats
    |> Chance.of_kind kind
    |> Float.sub_if Check.winter 0.1

  let roll_res kind =
    let (a, b) = Nation.ranges_of kind in
    Resource.(of_manp (roll a) <+ Supply (roll b))

  let to_res nats kind =
    if chance_of kind nats |> S.Dice.chance
    then roll_res kind |> bonuses kind
    else Resource.empty

  let from nats =
    Nation.(chosen nats |> set2map (to_res nats))
end
