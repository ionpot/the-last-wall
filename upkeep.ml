let make w =
  let s = ~-(min w.supplies w.manpower) in
  let m = min (w.supplies - w.manpower) 0 in
  Outcome.make s m
