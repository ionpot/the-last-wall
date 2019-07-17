type kind = Ballista | Cavalry | Cyclops | Demon | Dervish | Harpy | Knight | Men | Orc | Ranger | Skeleton | Templar
type report = (kind * Defs.count) list
type sum_report = (Defs.count * kind list)

let attacks = [Skeleton; Orc; Demon; Harpy; Cyclops]
let defends = [Men; Dervish; Cavalry; Ranger; Templar; Ballista; Knight]

let barrage = [Men; Ranger]
let cavalry = [Cavalry; Knight]
let holy = [Dervish; Ranger; Templar]
let infantry = [Men; Ranger; Templar; Dervish]
let revive = infantry
let work = [Men; Dervish]

let abundance_of = function
  | Cyclops -> 0.05
  | Demon -> 0.3
  | Harpy -> 0.15
  | Orc -> 0.6
  | Skeleton -> 1.25
  | _ -> 0.

let chance_of = function
  | Cyclops -> -.0.8
  | Demon -> 0.4
  | Orc -> 0.6
  | Skeleton -> 0.8
  | _ -> 0.

let chance_growth_of = function
  | Cyclops -> 0.1
  | _ -> 0.05

let base_power = function
  | Cyclops -> 5.
  | Harpy | Knight -> 4.
  | Ballista | Cavalry | Demon | Ranger | Templar -> 2.
  | Dervish | Men | Orc -> 1.
  | Skeleton -> 0.5

let hit_chance = function
  | Ballista -> 0.1
  | Dervish -> 0.3
  | Knight -> 0.8
  | Ranger -> 0.2
  | Templar -> 0.5
  | _ -> 1.

module Expr = struct
  type t = kind * Defs.count
  let add = Pair.eq_map (+)
  let count = snd
  let has_count t = count t > 0
  let is = Pair.fst_is
  let kind = fst
  let make n k = (k, n)
  let map_count = Pair.snd_map
  let mul n t = map_count (( * ) n) t
  let of_pair p = p
  let power (k, n) = Defs.to_power n (base_power k)
  let power_base (k, _) = base_power k
  let set_count = Pair.snd_set
  let sub = Fn.flip (-) |> Pair.eq_map
end

type t = Expr.t list

let empty = []

let make n kind = [Expr.make n kind]

module Cost = struct
  let of_kind = function
    | Ballista -> make 2 Men
    | Cavalry -> make 1 Men
    | Knight -> make 1 Cavalry
    | Ranger
    | Templar -> make 1 Dervish
    | _ -> empty

  let from n kind =
    of_kind kind |> List.map (Expr.mul n)

  let supply = function
    | Templar -> 2
    | Knight -> 10
    | Ballista -> 12
    | _ -> 1

  let upkeep = function
    | Knight -> 3
    | Ballista -> 2
    | _ -> 1

  let upkeep_of_expr e =
    Expr.count e * upkeep (Expr.kind e)

  let from_upkeep kind sup =
    Number.div sup (upkeep kind)

  let to_upkeep kind n =
    n * upkeep kind
end

module Ls = struct
  let clean t =
    List.filter Expr.has_count t

  let count_all t =
    List.map Expr.count t
    |> Listx.sum

  let discard kind t =
    Listx.discard (Expr.is kind) t

  let filter kind t =
    List.filter (Expr.is kind) t

  let filter_ls kinds t =
    List.filter (fun e -> List.mem (Expr.kind e) kinds) t

  let count kind t =
    filter kind t |> count_all

  let count_ls kinds t =
    filter_ls kinds t |> count_all

  let has kind t =
    List.exists (Expr.is kind) t

  let map_count f t =
    List.map (Expr.map_count f) t

  let add expr t =
    if has (Expr.kind expr) t
    then List.map (Expr.add expr) t
    else expr :: t

  let sub expr t =
    List.map (Expr.sub expr) t
end

let count = Ls.count
let count_all = Ls.count_all
let count_cavalry = Ls.count_ls cavalry
let count_holy = Ls.count_ls holy
let count_infantry = Ls.count_ls infantry

let min_promotable ls t =
  ls |> List.map (fun (k, n) -> n, count k t)
  |> List.map (fun (n, total) -> Number.div total n)
  |> Listx.min_of

let affordable kind cap t =
  match Cost.of_kind kind with
  | [] -> cap
  | ls -> min_promotable ls t |> min cap

let promotable kind t =
  match Cost.of_kind kind with
  | [] -> 0
  | ls -> min_promotable ls t

module Dr = struct
  let of_kind = function
    | Knight -> 0.004
    | Cavalry | Harpy -> 0.002
    | _ -> 0.

  let from n kind =
    Defs.to_power n (of_kind kind)

  let of_expr e =
    from (Expr.count e) (Expr.kind e)

  let from_kind k t =
    from (count k t) k

  let cavalry t =
    cavalry
    |> List.map (fun k -> from_kind k t)
    |> Listx.sumf

  let harpy t =
    from_kind Harpy t
    |> Float.floor_by 0.01
end

let find n kind t =
  let found = count kind t in
  min n found

let has = Ls.has

let has_base_power p t =
  List.map Expr.power_base t
  |> List.exists ((<=) p)

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

let revivable t =
  Ls.filter_ls revive t

let upkeep t =
  List.map Cost.upkeep_of_expr t
  |> Listx.sum

let workforce = powers_of work

let add n kind t =
  Ls.add (Expr.make n kind) t

let clean = Ls.clean

let combine t t' =
  List.fold_left (Fn.flip Ls.add) t t'

let reduce t t' =
  List.fold_left (Fn.flip Ls.sub) t' t
  |> Ls.clean

let rm = Ls.discard

let starve supply t =
  let ns =
    List.map (fun k -> count k t) defends
    |> List.map2 Cost.to_upkeep defends
    |> Listx.map_with Number.take supply
    |> List.map2 Cost.from_upkeep defends
  in
  List.map2 Expr.make ns defends
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

module Picked = struct
  type dist = (kind * Defs.power) list

  let group_by t ls =
    List.map (fun k -> k, Ls.count k ls) (kinds_of t)

  let groupf_by t ls =
    let count k =
      List.filter (Pair.fst_is k) ls
      |> List.map snd |> Listx.sumf
    in
    List.map (fun k -> k, count k) (kinds_of t)

  let to_units ls =
    ls |> List.map (fun (k, n) -> k, n /. base_power k)
    |> List.map (fun (k, n) -> Expr.make (truncate n) k)
end

module Dist (Dice : Dice.S) = struct
  module Pick = Pick.With(struct
    include Ops(Pick.Float)(Dice)
    let choose pairs =
      let probs = List.map (Pair.fst_to hit_chance) pairs in
      Dice.pick_w probs pairs
    let roll (k, n) = Dice.rollf n
    let trim cap (k, n) = min cap n
  end)

  let from power t =
    List.map (fun expr -> Expr.(kind expr, power expr)) t
    |> Pick.from power
end

module Fill (Dice : Dice.S) = struct
  module Pick = Pick.With(struct
    include Ops(Pick.Float)(Dice)
    let roll (k, n) = Dice.rollf n |> Float.floor_by (base_power k)
    let trim cap (k, n) = min cap n |> Float.floor_by (base_power k)
  end)

  let fn power t =
    List.map (fun expr -> Expr.(kind expr, power expr)) t
    |> Pick.from power
    |> Picked.groupf_by t
    |> Picked.to_units
    |> Ls.clean

  let from pwr t =
    if pwr > power t then t else fn pwr t
end

module FillCount (Dice : Dice.S) = struct
  module Pick = Pick.With(struct
    include Ops(Pick.Int)(Dice)
    let roll (k, n) = Dice.roll n
    let trim cap (k, n) = min cap n
  end)

  let fn total t =
    List.map (fun expr -> Expr.(kind expr, count expr)) t
    |> Pick.from total
    |> Picked.group_by t

  let from total t =
    if total > count_all t then t else fn total t
end

module Report (Dice : Dice.S) = struct
  let try_round x =
    if x > 10 then 10 * Dice.round (0.1 *. float x) else x

  let from t =
    List.map (Expr.map_count try_round) t

  let sum_from t =
    try_round (count_all t), (kinds_of t)
end
