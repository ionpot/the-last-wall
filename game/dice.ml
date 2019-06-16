module type From = sig
  val bool : unit -> bool
  val float : float -> float
  val int : int -> int
end

module type S = sig
  val between : int -> int -> int
  val betweenf : float -> float -> float
  val between_try : int -> int -> int
  val betweenf_try : float -> float -> float
  val betweenf_times : int -> float -> float -> float
  val betweenf_times_try : int -> float -> float -> float
  val chance : float -> bool
  val deviate : int -> int -> int
  val index : int -> int
  val pick : 'a list -> 'a
  val pick_w : float list -> 'a list -> 'a
  val roll : int -> int
  val rollf : float -> float
  val round : float -> int
  val yes : unit -> bool
end

module From (M : From) : S = struct
  let index = M.int

  let roll i =
    if i > 0 then index i + 1 else 0

  let rollf x =
    if x > 1. then M.float (x -. 1.) +. 1. else x

  let between x y =
    let d = y - x in
    x + index (d + 1)

  let between_try x y =
    if y > x then between x y else y

  let betweenf x y =
    x +. M.float y

  let betweenf_try x y =
    if y > x then betweenf x y else y

  let betweenf_times n x y =
    let n' = float n in
    betweenf (x *. n') (y *. n')

  let betweenf_times_try n x y =
    let n' = float n in
    betweenf_try (x *. n') (y *. n')

  let chance fl =
    if fl >= 1. then true
    else if fl < 0.0001 then false
    else M.float 1.0 < fl

  let deviate x y =
    let a = x - y in
    let b = x + y in
    between a b

  let pick ls =
    List.length ls |> index |> List.nth ls

  let pick_w probs ls =
    let num = Listx.sumf probs |> M.float in
    Listx.pick num probs ls

  let yes = M.bool

  let round x =
    let f = if yes () then floor else ceil in
    truncate (f x)
end
