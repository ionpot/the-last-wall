type kind = Ballista | Berserker | Cavalry | Cyclops | Demon | Dervish | Dullahan | Harcher | Harpy | Knight | Men | Merc | Novice | Orc | Ranger | Skeleton | Templar | Veteran

module Kind = struct
  type t = kind
  let compare = compare
end

module Map = Map.Make(Kind)
module Mapx = Mapx.Make(Map)
module Ops = Mapx.Int
module Set = Set.Make(Kind)

type report = (kind * Defs.count) list
type sum_report = (Defs.count * Set.t)

let attacks = [Skeleton; Orc; Demon; Harpy; Cyclops; Dullahan]
let starve_order = [Men; Novice; Dervish; Berserker; Cavalry; Harcher; Veteran; Ranger; Templar; Merc; Ballista; Knight]

module Attr = struct
  type t = kind -> bool
  let can_barrage = function
    | Harcher | Men | Merc | Ranger | Veteran -> true
    | _ -> false
  let can_build = function
    | Dervish | Men | Novice | Veteran -> true
    | _ -> false
  let can_fear = (=) Dullahan
  let can_heal = (=) Templar
  let can_reflect = function
    | Berserker | Harcher -> true
    | _ -> false
  let is_cavalry = function
    | Cavalry | Harcher | Knight -> true
    | _ -> false
  let not_cavalry = Fun.negate is_cavalry
  let is_holy = function
    | Dervish | Novice | Ranger | Templar -> true
    | _ -> false
  let is_siege = (=) Ballista
  let not_siege = Fun.negate is_siege
  let is_infantry k = not_cavalry k && not_siege k
  let is_infectable = not_siege
  let is_revivable = not_siege
  let is_undead = function
    | Skeleton | Dullahan -> true
    | _ -> false
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
    | Dullahan -> 0.01
    | Knight -> 0.004
    | Cavalry | Harpy -> 0.002
    | _ -> 0.

  let hit_chance = function
    | Ballista -> 0.25
    | Dervish | Harcher | Ranger -> 0.5
    | _ -> 1.

  let power = function
    | Dullahan -> 7.
    | Cyclops -> 5.
    | Harpy | Knight -> 4.
    | Templar -> 3.
    | Ballista | Berserker | Cavalry | Demon | Dervish | Harcher | Merc | Ranger | Veteran -> 2.
    | Men | Novice | Orc -> 1.
    | Skeleton -> 0.5

  let revive = function
    | Dervish -> 1.
    | Novice -> 0.5
    | _ -> 0.

  let supply_cost = function
    | Berserker -> 0
    | Merc
    | Dervish -> 2
    | Templar -> 3
    | Knight -> 10
    | Ballista -> 12
    | _ -> 1

  let upkeep_cost = function
    | Knight -> 3
    | Ballista | Merc | Veteran -> 2
    | _ -> 1
end

let from_upkeep kind sup =
  Number.div sup (Base.upkeep_cost kind)

let to_upkeep kind n =
  n * Base.upkeep_cost kind

type t = Defs.count Map.t

let empty : t = Map.empty

let make n kind =
  if n > 0 then Map.singleton kind n else empty

let is_empty = Map.is_empty

let discard = Mapx.discardk
let filter = Mapx.filterk

module Promote = struct
  let needs = function
    | Harcher
    | Knight -> Cavalry
    | Dervish
    | Ranger
    | Templar -> Novice
    | _ -> Men

  let amount = function
    | Ballista
    | Berserker -> 2
    | Cavalry
    | Dervish
    | Harcher
    | Knight
    | Ranger
    | Templar
    | Veteran -> 1
    | _ -> 0

  let affordable kind cap t =
    let n = amount kind in
    if n > 0 then
      let k = needs kind in
      if Map.mem k t
      then min cap (Map.find k t / n)
      else 0
    else cap

  let cost n kind =
    make (n * amount kind) (needs kind)

  let max kind t =
    let n = amount kind in
    let k = needs kind in
    if n > 0 && Map.mem k t
    then Map.find k t / n
    else 0
end

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

let ratio_of kind t =
  let n = count kind t in
  let sum = Ops.sum t in
  Float.div (float n) (float sum)

let report t =
  Map.bindings t

let upkeep t =
  Map.mapi to_upkeep t |> count_all

let add n kind t =
  if n > 0 then Map.add kind (n + count kind t) t else t

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

module Fill (Dice : Dice.S) = struct
  module Roll = Dice.Map(Map)
  module Pick = Pick.With(struct
    module Cap = Pick.Int
    module Map = Map
    module Type = Cap
    type map = Type.t Map.t
    type step = Cap.t * Type.t
    let choose = Roll.key
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
