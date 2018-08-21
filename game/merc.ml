let chance x =
  if Dice.chance 0.8
  then Some (Resource.of_manp x)
  else None

let roll () =
  let x = Dice.between 10 30 in
  chance x
