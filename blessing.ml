let make deity =
  let n = Dice.deviate 10 5 in
  match deity with
  | Sitera -> Outcome.make n 0
  | Elanis -> Outcome.make 0 n
