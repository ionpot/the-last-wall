let ceil_by ratio x =
  ratio *. ceil (x /. ratio)

let div a b =
  if b > 0. then a /. b else a

let floor_by ratio x =
  ratio *. floor (x /. ratio)

let increase x ratio =
  x +. x *. ratio

let reduce x ratio =
  x -. x *. ratio

let sub a b =
  if a < b then 0. else a -. b

let sub_opt a b =
  if a > b then Some (a -. b) else None
