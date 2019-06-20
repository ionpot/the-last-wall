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
  val roll : pair -> Defs.power * Num.t
  val trim : Defs.power -> pair -> Num.t
end

module With (S : Ops) = struct
  let zero = S.Num.zero

  let add = Pair.eq_map S.Num.add
  let sub = Pair.eq_map (Fn.flip S.Num.sub)

  let clean = List.filter (fun (_, x) -> x > zero)

  let picked pair pairs output =
    List.map (sub pair) pairs,
    List.map (add pair) output

  let pick cap pairs output =
    let pair = S.choose pairs in
    let key = fst pair in
    let pwr, num = S.roll pair in
    let pairs', output' =
      if num > zero
      then picked (key, num) pairs output
      else pairs, output
    in
    cap -. pwr, pairs', output'

  let trim pairs cap =
    pairs
    |> List.map (fun pair -> fst pair, S.trim cap pair)
    |> clean

  let rec start (cap, pairs, output) =
    match trim pairs cap with
    | [] -> output
    | pairs' -> start (pick cap pairs' output)

  let from cap pairs =
    let output = List.map (Pair.snd_set zero) pairs in
    start (cap, pairs, output)
    |> clean
end
