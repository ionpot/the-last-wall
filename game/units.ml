type kind = Cavalry | Demon | Dervish | Harpy | Men | Orc | Skeleton
type report = (Defs.count * kind) list
type sum_report = (Defs.count * kind list)

let attacks = [Skeleton; Orc; Demon; Harpy]
let defends = [Men; Cavalry; Dervish]

let abundance_of = function
  | Demon -> 0.3
  | Harpy -> 0.15
  | Orc -> 0.6
  | Skeleton -> 1.25
  | _ -> 0.

let chance_of = function
  | Demon -> 0.4
  | Orc -> 0.6
  | Skeleton -> 0.8
  | _ -> 0.

let cost_of = function
  | Cavalry
  | Dervish
  | Men -> 1
  | _ -> 0

let base_power = function
  | Harpy -> 4.
  | Cavalry | Demon -> 2.
  | Dervish | Men | Orc -> 1.
  | Skeleton -> 0.5

module Expr = struct
  type t = (Defs.count * kind)
  let add (n, k) (n', k') =
    if k = k' then n + n', k else (n', k')
  let count = fst
  let cost (n, k) = n * cost_of k
  let has_count (n, _) = n > 0
  let is kind (_, k) = k = kind
  let kind = snd
  let make n k = (n, k)
  let map_count f (n, k) = f n, k
  let power (n, k) = Defs.to_power n (base_power k)
  let power_base (_, k) = base_power k
  let set_count n (_, k) = n, k
  let sub (n, k) (n', k') =
    if k = k' then Number.sub n' n, k else (n', k')
end

type t = Expr.t list

let empty = []

let make n kind = [n, kind]

module Ls = struct
  let clean t =
    List.filter Expr.has_count t

  let filter kind t =
    List.filter (Expr.is kind) t

  let has kind t =
    List.exists (Expr.is kind) t

  let add expr t =
    if has (Expr.kind expr) t
    then List.map (Expr.add expr) t
    else expr :: t

  let sub expr t =
    List.map (Expr.sub expr) t
end

let count kind t =
  match Ls.filter kind t with
  | [] -> 0
  | expr :: _ -> Expr.count expr

let count_all t =
  List.map Expr.count t
  |> Listx.sum

let find n kind t =
  let found = count kind t in
  min n found

let has = Ls.has

let kinds_of t =
  List.map Expr.kind t

let power t =
  List.map Expr.power t
  |> Listx.sumf

let power_of kind t =
  Ls.filter kind t |> power

let ratio kind1 kind2 t =
  let a = count kind1 t in
  let b = count kind2 t in
  float a /. float b

let report t = t

let starve supply t =
  let ns =
    List.map (fun k -> count k t) defends
    |> Listx.map_with Number.take supply
  in List.map2 Expr.make ns defends
  |> Ls.clean

let upkeep t =
  List.map Expr.cost t
  |> Listx.sum

let add n kind t =
  Ls.add (Expr.make n kind) t

let sub n kind t =
  Ls.sub (Expr.make n kind) t
  |> Ls.clean

let combine t t' =
  List.fold_left (fun t'' e -> Ls.add e t'') t t'

let reduce t t' =
  List.fold_left (fun t'' e -> Ls.sub e t'') t' t
  |> Ls.clean

module Roll (Dice : Dice.S) = struct
  module Pick = Pick.With(Dice)

  let try_round x =
    if x > 10 then 10 * Dice.round (0.1 *. float x) else x

  let report t =
    List.map (Expr.map_count try_round) t

  let pick power t =
    List.map (fun expr -> Expr.(count expr, power_base expr)) t
    |> Pick.random power
    |> List.map2 (fun expr n -> Expr.set_count n expr) t

  let sum_report t =
    try_round (count_all t), (kinds_of t)
end
