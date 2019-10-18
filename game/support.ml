module Chance = Nation.Chance
module Map = Nation.Map
module Set = Nation.Set

type chances = Chance.t
type t = Resource.t Nation.Map.t

let is_empty k t =
  Map.mem k t && Map.find k t = Resource.empty

let sum t =
  let f _ = Resource.(++) in
  Map.fold f t Resource.empty

let to_set t =
  let m = Map.filter (fun _ -> (<>) Resource.empty) t in
  Map.fold (fun k _ s -> Set.add k s) m Set.empty

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
    to_set t |> Nation.set_aided |> S.Nation.map
end

module Roll (S : State.S) = struct
  module Check = Check(S)

  let bonuses kind res =
    let trade = Check.has_trade kind |> Number.if_ok 10 in
    Resource.(res ++ of_supp trade)

  let roll (a, b) = S.Dice.between a b

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
