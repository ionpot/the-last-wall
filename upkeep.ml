let of_wall (w: Wall.t) =
  let (s, m) = (w.supplies, w.manpower) in
  let sup = -(min s m) in
  let man = min (s - m) 0 in
  Outcome.Both (sup, man)
