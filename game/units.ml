type kind = Cavalry | Demon | Dervish | Harpy | Men | Orc | Ranger | Skeleton | Templar
type report = (Defs.count * kind) list
type sum_report = (Defs.count * kind list)

let attacks = [Skeleton; Orc; Demon; Harpy]
let defends = [Men; Cavalry; Ranger; Templar; Dervish]
let barrage = [Men; Ranger]
let temple = [Dervish; Ranger; Templar]
let work = [Men; Dervish]

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
  | Ranger
  | Templar -> 2
  | _ -> 0

let base_power = function
  | Harpy -> 4.
  | Cavalry | Demon | Ranger | Templar -> 2.
  | Dervish | Men | Orc -> 1.
  | Skeleton -> 0.5

let hit_chance = function
  | Dervish
  | Ranger -> 0.2
  | Templar -> 0.5
  | _ -> 1.

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

  let discard kind t =
    Listx.discard (Expr.is kind) t

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

let in_temple t =
  List.map (fun k -> count k t) temple
  |> Listx.sum

let kinds_of t =
  List.map Expr.kind t

let power t =
  List.map Expr.power t
  |> Listx.sumf

let power_of kind t =
  Ls.filter kind t |> power

let powers_of kinds t =
  kinds
  |> List.map (fun k -> power_of k t)
  |> Listx.sumf

let barrage_power t =
  powers_of barrage t *. 0.05

let ratio kind1 kind2 t =
  let a = count kind1 t in
  let b = count kind2 t in
  float a /. float b

let report t = t

let upkeep t =
  List.map Expr.cost t
  |> Listx.sum

let workforce = powers_of work

let add n kind t =
  Ls.add (Expr.make n kind) t

let combine t t' =
  List.fold_left (Fn.flip Ls.add) t t'

let reduce t t' =
  List.fold_left (Fn.flip Ls.sub) t' t
  |> Ls.clean

let rm = Ls.discard

let starve supply t =
  let ns =
    List.map (fun k -> count k t) defends
    |> Listx.map_with Number.take supply
  in List.map2 Expr.make ns defends
  |> Ls.clean

let sub n kind t =
  Ls.sub (Expr.make n kind) t
  |> Ls.clean

module Ops (Num : Pick.Num) (Dice : Dice.S) = struct
  module Num = Num
  type key = kind
  type pair = key * Num.t
  let choose = Dice.pick
end

let check_pick fn pwr t =
  if pwr > power t then t else fn pwr t

module Damage = struct
  let accept n = n, n
  let heal pwr n = n, Float.floor_by pwr n
  let handle kind =
    if kind = Templar
    then heal (base_power kind)
    else accept
end

module Dist (Dice : Dice.S) = struct
  module Pick = Pick.With(struct
    include Ops(Pick.Float)(Dice)
    let choose pairs =
      let probs = List.map (Pair.fst_to hit_chance) pairs in
      Dice.pick_w probs pairs
    let roll (k, n) = Dice.rollf n |> Damage.handle k
    let trim cap (k, n) = min cap n
  end)

  let fn power t =
    List.map (fun expr -> Expr.(kind expr, power expr)) t
    |> Pick.from power
    |> List.map (fun (k, n) ->
        let n' = truncate (n /. base_power k) in
        Expr.make n' k)

  let from = check_pick fn
end

module Fill (Dice : Dice.S) = struct
  module Pick = Pick.With(struct
    include Ops(Pick.Int)(Dice)
    let roll (k, n) =
      let n' = Dice.roll n in
      let pwr = Expr.(make n' k |> power) in
      pwr, n'
    let trim cap (k, n) =
      let power = base_power k in
      min n (if power > 0. then truncate (cap /. power) else n)
  end)

  let fn power t =
    List.map (fun expr -> Expr.(kind expr, count expr)) t
    |> Pick.from power
    |> List.map (fun (k, n) -> Expr.make n k)

  let from = check_pick fn
end

module Report (Dice : Dice.S) = struct
  let try_round x =
    if x > 10 then 10 * Dice.round (0.1 *. float x) else x

  let from t =
    List.map (Expr.map_count try_round) t

  let sum_from t =
    try_round (count_all t), (kinds_of t)
end
