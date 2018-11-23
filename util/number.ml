let sub a b =
  max 0 (a - b)

let deduce a b =
  if a < 1
  then a, b
  else sub a b, sub b a
