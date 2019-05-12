module type From = sig
  val bool : unit -> bool
  val float : float -> float
  val int : int -> int
end

module type S = sig
  val between : int -> int -> int
  val chance : float -> bool
  val deviate : int -> int -> int
  val index : int -> int
  val pick : 'a list -> 'a
  val roll : int -> int
  val rollf : float -> float
  val round : float -> int
  val yes : unit -> bool
end

module From (M : From) : S = struct
  let roll i =
    M.int i + 1

  let rollf fl =
    M.float (fl -. 1.) +. 1.

  let between x y =
    let d = y - x in
    x + roll d

  let chance fl =
    if fl >= 1. then true
    else if fl < 0.0001 then false
    else M.float 1.0 < fl

  let deviate x y =
    let a = x - y in
    let b = x + y in
    between a b

  let index = M.int

  let pick ls =
    List.length ls |> M.int |> List.nth ls

  let yes = M.bool

  let round x =
    let f = if yes () then floor else ceil in
    truncate (f x)
end
