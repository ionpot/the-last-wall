type report =
  | Accurate of Units.report
  | Vague of Units.sum_report

let kinds = Units.([Skeleton; Orc; Demon])

let abundance_of = function
  | Units.Skeleton -> 1.25
  | Units.Orc -> 0.6
  | Units.Demon -> 0.3
  | _ -> 0.

let chance_of = function
  | Units.Skeleton -> 0.8
  | Units.Orc -> 0.6
  | Units.Demon -> 0.4
  | _ -> 0.

module Roll (Dice : Dice.S) = struct
  module Roll = Units.Roll(Dice)

  let can_spawn turn kind =
    let a = 0.1 *. float (Number.sub turn 1) in
    let b = chance_of kind in
    Dice.chance (a +. b)

  let roll_count turn kind =
    let abundance = abundance_of kind in
    let minimum = 10. *. abundance in
    let amount =
      let x = 1.3 *. float (turn + 3) in
      abundance *. x *. log x
    in
    let x = ceil (minimum +. amount) |> truncate in
    Dice.deviate x (x / 4)

  let attack turn =
    let add t kind = Units.add (roll_count turn kind) kind t in
    List.filter (can_spawn turn) kinds
    |> List.fold_left add Units.empty

  let report scouting t =
    if scouting
    then Accurate (Roll.report t)
    else Vague (Roll.sum_report t)
end
