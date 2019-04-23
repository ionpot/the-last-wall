type report =
  | Accurate of Units.report
  | Vague of Units.sum_report

module Roll (Dice : Dice.S) = struct
  module Roll = Units.Roll(Dice)

  let can_spawn turn kind =
    let a = 0.1 *. float (Number.sub turn 1) in
    let b = Units.chance_of kind in
    Dice.chance (a +. b)

  let roll_count turn kind =
    let abundance = Units.abundance_of kind in
    let minimum = 10. *. abundance in
    let amount =
      let x = 1.3 *. float (turn + 3) in
      abundance *. x *. log x
    in
    let x = ceil (minimum +. amount) |> truncate in
    Dice.deviate x (x / 4)

  let attack turn =
    let add t kind = Units.add (roll_count turn kind) kind t in
    List.filter (can_spawn turn) Units.attacks
    |> List.fold_left add Units.empty

  let report scouting t =
    if scouting
    then Accurate (Roll.report t)
    else Vague (Roll.sum_report t)
end
