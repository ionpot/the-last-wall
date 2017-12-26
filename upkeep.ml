let make w =
  let s = -~w.manpower in
  let m = min (w.supplies - w.manpower) 0 in
  Outcome.make s m
