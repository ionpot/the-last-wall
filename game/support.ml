module Chance = Nation.Chance
module Map = Nation.Map

type chances = Chance.t
type t = Resource.t Nation.Map.t

let is_empty k t =
  Map.mem k t && Map.find k t = Resource.empty

let sum t =
  let f _ = Resource.(++) in
  Map.fold f t Resource.empty

module Check (S : State.S) = struct
  let starved = S.Starved.return Units.count_all
  let starvation = Float.times starved 0.01
  let winter = S.Month.check Month.is_winter
  let has_trade kind =
    S.Build.check Build.(has_trade kind)
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

  let cap_of kind =
    Chance.base
    |> Float.add_if (Check.has_trade kind) 0.1

  let cap kind =
    Chance.cap_at (cap_of kind) kind

  let starved =
    Chance.sub Check.starvation

  let chances t cmap =
    let f cmap kind =
      adjust kind t cmap
      |> boost kind
      |> cap kind
      |> starved kind
    in
    List.fold_left f cmap Nation.kinds
end

module Roll (S : State.S) = struct
  module Check = Check(S)

  let bonuses kind res =
    let leader = S.Leader.return Leader.res_bonus_of in
    let trade = Check.has_trade kind |> Number.if_ok 10 in
    Resource.(res ++ leader ++ of_supp trade)

  let roll (a, b) = S.Dice.between a b

  let chance_of kind nats =
    Nation.chances nats
    |> Chance.of_kind kind
    |> Float.sub_if Check.winter 0.1
    |> Float.sub_by Check.starvation

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
