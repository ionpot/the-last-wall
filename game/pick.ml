let invalid ls =
  List.map string_of_int ls
  |> String.concat ", "
  |> Printf.sprintf "invalid list [%s]"
  |> failwith

module type Num = sig
  type t
  val zero : t
  val add : t -> t -> t
  val sub : t -> t -> t
end

module Int = struct
  type t = int let zero = 0 let add = (+) let sub = (-)
end

module Float = struct
  type t = float let zero = 0. let add = (+.) let sub = (-.)
end

module type Ops = sig
  module Num : Num
  type key
  type pair = key * Num.t
  val choose : pair list -> pair
  val damage : pair -> Defs.power
  val roll : Defs.power -> pair -> Num.t
  val trim : Defs.power -> pair -> Num.t
end

module With (S : Ops) = struct
  let zero = S.Num.zero

  let add (a, x) (b, y) =
    if a = b then (a, S.Num.add x y) else (b, y)

  let sub (a, x) (b, y) =
    if a = b then (a, S.Num.sub y x) else (b, y)

  let picked pair pairs output =
    List.map (sub pair) pairs, (pair :: output)

  let pick cap pairs output =
    let pair = S.choose pairs in
    let count = S.roll cap pair in
    let pair' = fst pair, count in
    let damage = S.damage pair' in
    let pairs', output' =
      if count = zero then pairs, output
      else picked pair' pairs output
    in cap -. damage, pairs', output'

  let sort pairs output =
    let sum pair =
      List.fold_left (Fn.flip add) (fst pair, zero) output
    in
    List.map sum pairs

  let trim pairs cap =
    pairs
    |> List.map (fun pair -> fst pair, S.trim cap pair)
    |> List.filter (fun (_, x) -> x > zero)

  let rec start (cap, pairs, output) =
    match trim pairs cap with
    | [] -> output
    | pairs' -> start (pick cap pairs' output)

  let from cap pairs =
    start (cap, pairs, [])
    |> sort pairs
end
