let add_if cond a b =
  if cond then a +. b else b

let add_if_ptv a b =
  add_if (b > 0.) a b

let ceil_by ratio x =
  ratio *. ceil (x /. ratio)

let div a b =
  if b > 0. then a /. b else a

let floor_by ratio x =
  ratio *. floor (x /. ratio)

let if_ok x cond =
  if cond then x else 0.

let if_not x cond =
  if cond then 0. else x

let increase x ratio =
  x +. x *. ratio

let reduce x ratio =
  x -. x *. ratio

let ssub_by a b = b -. a

let ssub_if cond a b =
  if cond then b -. a else b

let sub a b =
  if a < b then 0. else a -. b

let sub_by a b = sub b a

let sub_if cond a b =
  if cond then sub b a else b

let sub_opt a b =
  if a > b then Some (a -. b) else None

let times n x =
  float n *. x
