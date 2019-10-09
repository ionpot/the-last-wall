module Chance = Nation.Chance
module Map = Nation.Map

type chances = Chance.t
type t = Resource.t Nation.Map.t

let is_empty k t =
  Map.mem k t && Map.find k t = Resource.empty

let sum t =
  let f _ = Resource.(++) in
  Map.fold f t Resource.empty

module Apply (S : State.S) = struct
  let adjust kind t cmap =
    (if is_empty kind t
    then Chance.reduce_by
    else Chance.increase_by) 0.1 kind cmap

  let boost kind cmap =
    if S.Build.check Build.(has_trade kind)
    then Chance.increase_by 0.05 kind cmap
    else cmap

  let chances t cmap =
    let f cmap kind = adjust kind t cmap |> boost kind in
    List.fold_left f cmap Nation.kinds
end

module Roll (S : State.S) = struct
  let has_trade kind =
    S.Build.check Build.(has_trade kind)

  let bonuses kind res =
    let leader = S.Leader.return Leader.res_bonus_of in
    let trade = has_trade kind |> Number.if_ok 10 in
    Resource.(res ++ leader ++ of_supp trade)

  let roll (a, b) = S.Dice.between a b

  let chance_of kind nats =
    let starved = S.Starved.return Units.count_all in
    let starvation = Float.times starved 0.01 in
    let winter = S.Month.check Month.is_winter in
    Nation.chances nats
    |> Chance.of_kind kind
    |> Float.sub_if winter 0.1
    |> Float.sub_by starvation

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
