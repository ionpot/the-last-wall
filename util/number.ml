let div a b =
  a / max b 1

let if_ok x cond =
  if cond then x else 0

let increase_by ratio x =
  truncate (Float.increase (float x) ratio)

let maybe x = function
  | Some a -> a
  | None -> x

let portion ratio x =
  truncate (float x *. ratio)

let ratio a b =
  float a /. float b

let reduce_by ratio x =
  truncate (Float.reduce (float x) ratio |> ceil)

let sub a b =
  max 0 (a - b)

let deduce a b =
  if a < 1
  then a, b
  else sub a b, sub b a

let sub_opt a b =
  let x = a - b in
  if x > 0 then Some x else None

let take a b =
  sub a b, min a b
