type kind = Ballista | Cavalry | Demon | Dervish | Harpy | Men | Orc | Ranger | Skeleton | Templar
type report = (kind * Defs.count) list
type sum_report = (Defs.count * kind list)

let attacks = [Skeleton; Orc; Demon; Harpy]
let defends = [Men; Cavalry; Ranger; Templar; Dervish; Ballista]
let barrage = [Men; Ranger]
let holy = [Dervish; Ranger; Templar]
let infantry = [Men; Ranger; Templar; Dervish]
let revive = infantry
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

let base_power = function
  | Harpy -> 4.
  | Ballista | Cavalry | Demon | Ranger | Templar -> 2.
  | Dervish | Men | Orc -> 1.
  | Skeleton -> 0.5

let hit_chance = function
  | Ballista -> 0.1
  | Dervish -> 0.3
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
    | Ranger
    | Templar -> make 1 Dervish
    | _ -> empty

  let from n kind =
    of_kind kind |> List.map (Expr.mul n)

  let supply = function
    | Dervish
    | Ranger -> 1
    | Templar -> 2
    | Ballista -> 12
    | _ -> 0

  let upkeep = function
    | Ballista -> 2
    | _ -> 1

  let upkeep_of_expr e =
    Expr.count e * upkeep (Expr.kind e)
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
    List.map (fun k -> filter k t) kinds
    |> List.concat

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
let count_holy = Ls.count_ls holy
let count_infantry = Ls.count_ls infantry

let affordable kind cap t =
  match Cost.of_kind kind with
  | [] -> cap
  | ls ->
      ls |> List.map (fun (k, n) -> n, count k t)
      |> List.map (fun (n, total) -> Number.div total n)
      |> Listx.min_of
      |> min cap

module Dr = struct
  let of_kind = function
    | Cavalry | Harpy -> 0.002
    | _ -> 0.

  let from n kind =
    Defs.to_power n (of_kind kind)

  let of_expr e =
    from (Expr.count e) (Expr.kind e)

  let from_kind k t =
    from (count k t) k

  let cavalry t =
    from_kind Cavalry t

  let harpy t =
    from_kind Harpy t
    |> Float.floor_by 0.01
end

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

module Ops (Total : Pick.Num) (Num : Pick.Num) (Dice : Dice.S) = struct
  module Num = Num
  module Total = Total
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
    include Ops(Pick.Float)(Pick.Float)(Dice)
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
    include Ops(Pick.Float)(Pick.Int)(Dice)
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
    |> List.map Expr.of_pair

  let from = check_pick fn
end

module FillCount (Dice : Dice.S) = struct
  module Pick = Pick.With(struct
    include Ops(Pick.Int)(Pick.Int)(Dice)
    let roll (k, n) = let n' = Dice.roll n in n', n'
    let trim cap (k, n) = min cap n
  end)

  let fn total t =
    List.map (fun expr -> Expr.(kind expr, count expr)) t
    |> Pick.from total
    |> List.map Expr.of_pair

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
