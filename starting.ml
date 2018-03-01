let make () =
  let sup = Dice.between 90 180 in
  let man = Dice.between 10 30 in
  Outcome.Both (sup, man)
