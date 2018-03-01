let of_deity (d: Deity.t) : Outcome.t =
  let r n = Dice.deviate n 5 in
  match d with
  | None -> None
  | Elanis -> Manpower (r 15)
  | Sitera -> Supply (r 15)
  | Sekrefir -> Both ((r 5), (r 5))
