open Defs

let chance x =
  if Dice.chance 0.8 then Some x else None

let roll supply =
  let x = Dice.between 10 30 in
  if x < supply then chance x else None
