let find enemies =
  let x = Dice.between 10 30 in
  Enemy.(find x Skeleton enemies)

let check enemies deity =
  if deity = Deity.Lerota
  then find enemies
  else None
