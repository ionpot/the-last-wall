type kind = Ballista | Cavalry | Cyclops | Demon | Dervish | Harpy | Knight | Men | Orc | Ranger | Skeleton | Templar
type report = (kind * Defs.count) list
type sum_report = (Defs.count * kind list)

let attacks = [Skeleton; Orc; Demon; Harpy; Cyclops]
let barrage = [Men; Ranger]
let cavalry = [Cavalry; Knight]
let holy = [Dervish; Ranger; Templar]
let infantry = [Men; Ranger; Templar; Dervish]
let revive = infantry
let starve_order = [Men; Dervish; Cavalry; Ranger; Templar; Ballista; Knight]
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

let toughness = function
  | Cyclops -> 2.
  | _ -> 1.

let from_power kind p =
  Float.div p (base_power kind)
  |> truncate

let to_power kind n =
  Defs.to_power n (base_power kind)

module Map = Map.Make(struct
  type t = kind
  let compare = compare
end)

type t = Defs.count Map.t

let empty : t = Map.empty

let make n kind =
  Map.singleton kind n

let make_ls ls =
  List.fold_left (fun t (k, n) -> Map.add k n t) empty ls

let filter_ls kinds t =
  Map.filter (fun k _ -> List.mem k kinds) t

module Ops = struct
  let add t_a t_b =
    Map.union (fun _ a b -> Some (a + b)) t_a t_b

  let div t_a t_b =
    let f _ a_opt = function
      | Some b ->
          if b > 0
          then Some (Number.maybe 0 a_opt / b)
          else None
      | None -> None
    in
    Map.merge f t_a t_b

  let min t =
    let cmp n = function
      | Some x -> Some (min x n)
      | None -> Some n
    in
    Map.fold (fun _ -> cmp) t None
    |> Number.maybe 0

  let mul n t =
    Map.map (( * ) n) t

  let powers t =
    Map.mapi to_power t

  let sub t_a t_b =
    let f _ a_opt = function
      | Some b -> Number.(sub_opt (maybe 0 a_opt) b)
      | None -> None
    in
    Map.merge f t_a t_b

  let sum t =
    Map.fold (fun _ -> (+)) t 0

  let sumf t =
    Map.fold (fun _ -> (+.)) t 0.
end

module Cost = struct
  let of_kind = function
    | Ballista -> make 2 Men
    | Cavalry -> make 1 Men
    | Knight -> make 1 Cavalry
    | Ranger
    | Templar -> make 1 Dervish
    | _ -> empty

  let from n kind =
    of_kind kind |> Ops.mul n

  let supply = function
    | Templar -> 2
    | Knight -> 10
    | Ballista -> 12
    | _ -> 1

  let upkeep = function
    | Knight -> 3
    | Ballista -> 2
    | _ -> 1

  let from_upkeep kind sup =
    Number.div sup (upkeep kind)

  let to_upkeep kind n =
    n * upkeep kind
end

let count kind t =
  try Map.find kind t with
  | Not_found -> 0

let count_all = Ops.sum

let count_ls ls t =
  filter_ls ls t |> count_all

let count_cavalry = count_ls cavalry
let count_holy = count_ls holy
let count_infantry = count_ls infantry

let affordable kind cap t =
  let m = Ops.div t (Cost.of_kind kind) in
  if Map.is_empty m then cap
  else Ops.min m |> min cap

let promotable kind t =
  let m = Ops.div t (Cost.of_kind kind) in
  if Map.is_empty m then 0
  else Ops.min m

module Dr = struct
  let of_kind = function
    | Knight -> 0.004
    | Cavalry | Harpy -> 0.002
    | _ -> 0.

  let to_power kind n =
    Defs.to_power n (of_kind kind)

  let cavalry t =
    filter_ls cavalry t
    |> Map.mapi to_power
    |> Ops.sumf

  let harpy t =
    count Harpy t
    |> to_power Harpy
    |> Float.floor_by 0.01
end

let find n kind t =
  let found = count kind t in
  min n found

let has kind t =
  count kind t > 0

let has_base_power p t =
  Map.exists (fun k _ -> base_power k >= p) t

let kinds_of t =
  Map.bindings t
  |> List.map fst

let power t =
  Ops.(powers t |> sumf)

let power_of kind t =
  to_power kind (count kind t)

let powers_of kinds t =
  filter_ls kinds t
  |> power

let barrage_power t =
  powers_of barrage t *. 0.05

let report = Map.bindings

let upkeep t =
  Map.mapi Cost.to_upkeep t |> count_all

let workforce = powers_of work

let add n kind t =
  Map.add kind (n + count kind t) t

let defending t =
  Map.remove Ballista t

let combine = Ops.add

let countered units t =
  Map.filter (fun k _ -> has_base_power (toughness k) units) t

let heal kind =
  Float.floor_by (base_power kind)

let reduce t t' =
  Ops.sub t' t

let revivable t =
  filter_ls revive t

let rm = Map.remove

let starve supply t =
  let f (sup, t') k =
    let cost = count k t |> Cost.to_upkeep k in
    let sup', cost' = Number.take sup cost in
    let n = Cost.from_upkeep k cost' in
    sup', if n > 0 then add n k t' else t'
  in
  List.fold_left f (supply, empty) starve_order
  |> snd

let sub n kind t =
  Map.update kind (function Some x -> Number.sub_opt x n | x -> x) t

let check_pick fn pwr t =
  if pwr > power t then t else fn pwr t

let pick_w t n =
  let key, _ = Map.min_binding t in
  let f k hit (k', n') =
    if n' > 0. then k, n' -. hit else k', n'
  in
  Map.fold f t (key, n) |> fst

module Dist (Dice : Dice.S) = struct
  module Pick = Pick.With(struct
    module Cap = Pick.Float
    module Map = Map
    module Type = Cap
    type step = Cap.t * Type.t
    let choose input =
      let probs = Map.mapi (fun k _ -> hit_chance k) input in
      Ops.sumf probs |> Dice.rollf |> pick_w probs
    let roll kind cap input =
      let cap = Map.find kind input |> min cap |> Dice.rollf in
      let sub = if kind = Templar then heal kind cap else cap in
      cap, sub
  end)

  let fn power t =
    let input = Ops.powers t in
    let output = Map.map (fun _ -> 0.) t in
    Pick.from power input output
    |> Map.mapi from_power

  let from = check_pick fn
end

let random_key roll t =
  let n = Map.cardinal t |> roll in
  let key, _ = Map.choose t in
  let f k _ (k', n') =
    if n' > 0 then k, pred n' else k', n'
  in
  Map.fold f t (key, n) |> fst

module Fill (Dice : Dice.S) = struct
  module Pick = Pick.With(struct
    module Cap = Pick.Float
    module Map = Map
    module Type = Pick.Int
    type step = Cap.t * Type.t
    let choose = random_key Dice.roll
    let roll kind cap t =
      let n = Map.find kind t |> min (from_power kind cap) |> Dice.roll in
      to_power kind n, n
  end)

  let fn power t =
    Pick.from power t empty

  let from = check_pick fn
end

module FillCount (Dice : Dice.S) = struct
  module Pick = Pick.With(struct
    module Cap = Pick.Int
    module Map = Map
    module Type = Cap
    type step = Cap.t * Type.t
    let choose = random_key Dice.roll
    let roll kind cap t =
      let n = Map.find kind t |> min cap |> Dice.roll in
      n, n
  end)

  let from total t =
    if total > count_all t then t
    else Pick.from total t empty
end

module Report (Dice : Dice.S) = struct
  let try_round x =
    if x > 10 then 10 * Dice.round (0.1 *. float x) else x

  let from t =
    Map.map try_round t
    |> report

  let sum_from t =
    try_round (count_all t), (kinds_of t)
end
