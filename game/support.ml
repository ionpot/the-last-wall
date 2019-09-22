module Chance = Nation.Chance

type chances = Chance.t
type t = (Nation.kind * Resource.t) list

let ls t = t

let sum =
  let f total (_, res) =
    Resource.(total ++ res)
  in
  List.fold_left f Resource.empty

let update_chances t chances =
  let f cmap (kind, res) =
    if res = Resource.empty
    then Chance.deduct kind cmap
    else Chance.reset kind cmap
  in
  List.fold_left f chances t

module Roll (S : State.S) = struct
  let starved = S.Starved.return Units.count_all
  let trade = S.Build.return Build.trade

  let bonuses kind res =
    let leader = S.Leader.return Leader.res_bonus_of in
    let trade = if trade = Nation.Boost kind then 10 else 0 in
    Resource.(res ++ leader ++ of_supp trade)

  let roll (a, b) = S.Dice.between a b

  let roll_chance kind nats =
    let starved = Defs.to_power starved 0.01 in
    let winter = S.Month.check Month.is_winter in
    Nation.chances nats
    |> Chance.of_kind kind
    |> Float.sub_if winter 0.1
    |> Float.sub_by starved
    |> S.Dice.chance

  let roll_res kind =
    let (a, b) = Nation.ranges_of kind in
    Resource.(of_manp (roll a) <+ Supply (roll b))

  let to_res kind nats =
    if trade = Certain kind || roll_chance kind nats
    then roll_res kind |> bonuses kind
    else Resource.empty

  let from nats =
    Nation.which nats
    |> List.map (fun kind -> kind, to_res kind nats)
end
