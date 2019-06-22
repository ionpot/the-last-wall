let increase_by ratio x =
  truncate (Float.increase (float x) ratio)

let reduce_by ratio x =
  truncate (Float.reduce (float x) ratio |> ceil)

let portion ratio x =
  truncate (float x *. ratio)

let ratio a b =
  float a /. float b

let sub a b =
  max 0 (a - b)

let deduce a b =
  if a < 1
  then a, b
  else sub a b, sub b a

let take a b =
  sub a b, min a b
