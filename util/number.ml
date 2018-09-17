let deduce a b =
  let d = a - b in
  if d > 0 then d, 0 else 0, ~-d
