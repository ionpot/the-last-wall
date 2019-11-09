type kind = Ballista | Berserker | Cavalry | Cyclops | Demon | Dervish | Harpy | Knight | Men | Merc | Orc | Ranger | Skeleton | Templar

module Kind = struct
  type t = kind
  let compare = compare
end

module Map = Map.Make(Kind)
module Set = Set.Make(Kind)

type report = (kind * Defs.count) list
type sum_report = (Defs.count * Set.t)

let attacks = [Skeleton; Orc; Demon; Harpy; Cyclops]
let starve_order = [Men; Dervish; Berserker; Cavalry; Ranger; Templar; Merc; Ballista; Knight]

module Attr = struct
  type t = kind -> bool
  let can_barrage = function
    | Men | Merc | Ranger -> true
    | _ -> false
  let can_build = function
    | Men | Dervish -> true
    | _ -> false
  let can_heal = (=) Templar
  let can_reflect = (=) Berserker
  let is_cavalry = function
    | Cavalry | Knight -> true
    | _ -> false
  let is_holy = function
    | Dervish | Ranger | Templar -> true
    | _ -> false
  let is_infantry = function
    | Berserker | Men | Merc | Dervish | Ranger | Templar -> true
    | _ -> false
  let is_infectable = (<>) Ballista
  let is_revivable = is_infantry
  let is_siege = (=) Ballista
end

module Base = struct
  let abundance = function
    | Cyclops -> 0.05
    | Demon -> 0.3
    | Harpy -> 0.15
    | Orc -> 0.6
    | Skeleton -> 1.25
    | _ -> 0.

  let chance = function
    | Cyclops -> -.0.8
    | Demon -> 0.4
    | Orc -> 0.6
    | Skeleton -> 0.8
    | _ -> 0.

  let chance_growth = function
    | Cyclops -> 0.1
    | _ -> 0.05

  let dr = function
    | Knight -> 0.004
    | Cavalry | Harpy -> 0.002
    | _ -> 0.

  let hit_chance = function
    | Ballista -> 0.1
    | Dervish -> 0.3
    | Knight -> 0.8
    | Ranger -> 0.2
    | Templar -> 0.5
    | _ -> 1.

  let power = function
    | Cyclops -> 5.
    | Harpy | Knight -> 4.
    | Ballista | Berserker | Cavalry | Demon | Merc | Ranger | Templar -> 2.
    | Dervish | Men | Orc -> 1.
    | Skeleton -> 0.5

  let supply_cost = function
    | Berserker -> 0
    | Merc
    | Templar -> 2
    | Knight -> 10
    | Ballista -> 12
    | _ -> 1

  let upkeep_cost = function
    | Knight -> 3
    | Ballista | Merc -> 2
    | _ -> 1
end

let from_upkeep kind sup =
  Number.div sup (Base.upkeep_cost kind)

let to_upkeep kind n =
  n * Base.upkeep_cost kind

type t = Defs.count Map.t

let empty : t = Map.empty

let make n kind =
  Map.singleton kind n

let is_empty = Map.is_empty

let discard attr t =
  Map.filter (fun k _ -> not (attr k)) t

let filter attr t =
  Map.filter (fun k _ -> attr k) t

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

  let sub t_a t_b =
    let f _ a_opt = function
      | Some b -> Number.(sub_opt (maybe 0 a_opt) b)
      | None -> a_opt
    in
    Map.merge f t_a t_b

  let sum t =
    Map.fold (fun _ -> (+)) t 0
end

let promotion_cost = function
  | Ballista
  | Berserker -> make 2 Men
  | Cavalry -> make 1 Men
  | Knight -> make 1 Cavalry
  | Ranger
  | Templar -> make 1 Dervish
  | _ -> empty

let affordable kind cap t =
  let u = Ops.div t (promotion_cost kind) in
  if is_empty u then cap
  else Ops.min u |> min cap

let cost n kind =
  promotion_cost kind |> Ops.mul n

let count kind t =
  if Map.mem kind t
  then Map.find kind t
  else 0

let count_all = Ops.sum

let filter_count attr t =
  filter attr t |> count_all

let find n kind t =
  let found = count kind t in
  min n found

let has kind t =
  count kind t > 0

let kinds_of t =
  let f k _ s = Set.add k s in
  Map.fold f t Set.empty

let promotable kind t =
  let u = Ops.div t (promotion_cost kind) in
  if is_empty u then 0
  else Ops.min u

let ratio_of kind t =
  let n = count kind t in
  let sum = Ops.sum t in
  Float.div (float n) (float sum)

let report t =
  Map.bindings t

let upkeep t =
  Map.mapi to_upkeep t |> count_all

let add n kind t =
  Map.add kind (n + count kind t) t

let combine = Ops.add

let only kind t =
  let n = count kind t in
  if n > 0 then make n kind else empty

let pop kind t =
  Map.partition (fun k _ -> k = kind) t

let reduce t t' =
  Ops.sub t' t

let split attr t =
  Map.partition (fun k _ -> attr k) t

let starve supply t =
  let f (sup, t') k =
    let cost = count k t |> to_upkeep k in
    let sup', cost' = Number.take sup cost in
    let n = from_upkeep k cost' in
    sup', if n > 0 then add n k t' else t'
  in
  List.fold_left f (supply, empty) starve_order
  |> snd

let sub n kind t =
  Map.update kind (function Some x -> Number.sub_opt x n | x -> x) t

module Roll (Dice : Dice.S) = struct
  let kind t =
    let n = Map.cardinal t |> Dice.roll in
    let key, _ = Map.choose t in
    let f k _ (k', n') =
      if n' > 0 then k, pred n' else k', n'
    in
    Map.fold f t (key, n) |> fst
end

module Fill (Dice : Dice.S) = struct
  module Roll = Roll(Dice)
  module Pick = Pick.With(struct
    module Cap = Pick.Int
    module Map = Map
    module Type = Cap
    type map = Type.t Map.t
    type step = Cap.t * Type.t
    let choose = Roll.kind
    let roll cap kind t =
      let n = Map.find kind t |> min cap |> Dice.roll in
      n, n
  end)

  let from total t =
    if total > count_all t then t, empty
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
