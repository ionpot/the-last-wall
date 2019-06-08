let floor_by ratio x =
  ratio *. floor (x *. 1. /. ratio)

let increase x ratio =
  x +. x *. ratio

let reduce x ratio =
  x -. x *. ratio

let sub a b =
  if a < b then 0. else a -. b
