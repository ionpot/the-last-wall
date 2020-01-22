module Chance = Nation.Chance
module Map = Nation.Map

type chances = Chance.t
type t = Nation.support

let is_empty k t =
  Map.mem k t && Map.find k t = Resource.empty

let sum t =
  let f _ = Resource.(++) in
  Map.fold f t Resource.empty

module Apply (S : State.S) = struct
  let nats = S.Nation.get ()

  let adjust kind t cmap =
    (if is_empty kind t
    then Chance.sub
    else Chance.add) 10 kind cmap

  let boost kind cmap =
    if Nation.has_trade kind nats
    then Chance.add 5 kind cmap
    else cmap

  let chances t cmap =
    let f cmap kind = adjust kind t cmap |> boost kind in
    List.fold_left f cmap Nation.kinds

  let value t =
    S.Nation.return (chances t |> Nation.map_chances)
    |> Nation.trim_chances
    |> Nation.set_support t
    |> S.Nation.set
end

module Roll (S : State.S) = struct
  module Bonus = Bonus.Make(S)

  let bonuses kind res = res
    |> Bonus.support_barracks kind
    |> Bonus.support_hekatium kind
    |> Bonus.support_trade kind

  let roll = S.Dice.range

  let chance_of kind nats =
    Nation.chances nats
    |> Chance.of_kind kind
    |> Bonus.support_winter

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
