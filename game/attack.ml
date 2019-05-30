type report =
  | Accurate of Units.report
  | Vague of Units.sum_report

module Make (S : State.S) = struct
  module Report = Units.Report(S.Dice)

  let can_regular turn kind =
    let a = 0.1 *. float (Number.sub turn 1) in
    let b = Units.chance_of kind in
    S.Dice.chance (a +. b)

  let can_spawn turn kind =
    if kind = Units.Harpy
    then S.Harpy.check S.Dice.chance
    else can_regular turn kind

  let roll_count turn kind =
    let abundance = Units.abundance_of kind in
    let minimum = 10. *. abundance in
    let amount =
      let x = 1.3 *. float (turn + 3) in
      abundance *. x *. log x
    in
    let x = ceil (minimum +. amount) |> truncate in
    S.Dice.deviate x (x / 4)

  let roll turn =
    let add t kind = Units.add (roll_count turn kind) kind t in
    List.filter (can_spawn turn) Units.attacks
    |> List.fold_left add Units.empty

  let report scouting t =
    if scouting
    then Accurate (Report.from t)
    else Vague (Report.sum_from t)
end
