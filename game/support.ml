module Chance = Nation.Chance

type chances = Chance.t
type t = (Nation.kind * Resource.t) list

let ls t = t

let sum =
  let f total (_, res) =
    Resource.(total ++ res)
  in
  List.fold_left f Resource.empty

module Apply (S : State.S) = struct
  let boost chances =
    let f cmap kind =
      if S.Build.check Build.(has_trade kind)
      then Chance.increase_by 0.05 kind cmap
      else cmap
    in
    List.fold_left f chances Nation.kinds

  let chances t chances =
    let empty = List.filter (fun (_, res) -> res = Resource.empty) t in
    let f cmap kind =
      (if List.mem_assoc kind empty
      then Chance.reduce_by
      else Chance.increase_by) 0.1 kind cmap
    in
    List.fold_left f chances Nation.kinds
    |> boost
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

  let to_res kind nats =
    if chance_of kind nats |> S.Dice.chance
    then roll_res kind |> bonuses kind
    else Resource.empty

  let from nats =
    Nation.which nats
    |> List.map (fun kind -> kind, to_res kind nats)
end
