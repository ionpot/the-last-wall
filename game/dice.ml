let index = Random.int

let roll i =
  if i > 0 then index i + 1 else 0

let rollf = Random.float

let between x y =
  let d = y - x in
  x + index (d + 1)

let between_try x y =
  if y > x then between x y else y

let betweenf x y =
  min y (x +. Random.float (y -. x))

let betweenf_try x y =
  if y > x then betweenf x y else y

let betweenf_times n x y =
  let n' = float n in
  betweenf (x *. n') (y *. n')

let percent x =
  index 100 < x

let betweenf_times_try n x y =
  let n' = float n in
  betweenf_try (x *. n') (y *. n')

let range (x, y) =
  between x y

let rangef_times_try n (x, y) =
  betweenf_times_try n x y

let ratio x =
  betweenf (1. /. x) 1.

let chance fl =
  if fl >= 1. then true
  else if fl < 0.0001 then false
  else Random.float 1.0 < fl

let deviate x y =
  let a = x - y in
  let b = x + y in
  between a b

let pick ls =
  List.length ls |> index |> List.nth ls

let pick_w probs ls =
  let num = Listx.sumf probs |> Random.float in
  Listx.pick num probs ls

let pop ls =
  let x = pick ls in
  x, Listx.rm x ls

let yes = Random.bool

let round x =
  let f = if yes () then floor else ceil in
  truncate (f x)

module Map (Map : Map.S) = struct
  module Mapx = Mapx.Make(Map)

  let key m =
    Map.cardinal m |> roll |> Mapx.nth m
end
