let make () =
  let a = Dice.deviate 10 5 in
  let b = Dice.deviate 5 5 in
  if Random.bool ()
  then Outcome.make a b
  else Outcome.make b a

let check () =
  Dice.chance 0.8
