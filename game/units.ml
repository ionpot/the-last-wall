type kind = Ballista | Berserker | Cavalry | Cyclops | Demon | Dervish | Dullahan | Harcher | Harpy | Knight | Mangonel | Marms | Men | Merc | Novice | Orc | Ranger | Skeleton | Templar | Veteran | Xbowman

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
let starve_order = [Men; Novice; Berserker; Cavalry; Veteran; Harcher; Ranger; Dervish; Templar; Merc; Xbowman; Marms; Mangonel; Ballista; Knight]

module Attr = struct
  type t = Set.t
  let all = Set.of_list starve_order
  let archer = Set.of_list [Harcher; Ranger; Xbowman]
  let barrage = Set.of_list [Men; Merc; Veteran] |> Set.union archer
  let barraged = Set.singleton Orc
  let build = Set.of_list [Dervish; Men; Novice; Veteran]
  let cavalry = Set.of_list [Cavalry; Harcher; Marms; Knight]
  let fear = Set.singleton Dullahan
  let flying = Set.singleton Harpy
  let heal = Set.singleton Templar
  let hit_run = Set.singleton Harcher
  let holy = Set.of_list [Dervish; Novice; Ranger; Templar]
  let reflect = Set.singleton Berserker
  let siege = Set.of_list [Ballista; Mangonel]
  let undead = Set.of_list [Skeleton; Dullahan]
  let infantry = Set.union cavalry siege |> Set.diff all
  let infectable = Set.diff all siege
  let revivable = Set.diff all siege
  let fold t f = Set.fold f t
  let is t k = Set.mem k t
  let set_of t = t
end

module Base = struct
  let abundance = function
    | Cyclops -> 0.05
    | Demon -> 0.3
    | Harpy -> 0.15
    | Orc -> 0.6
    | Skeleton -> 1.25
    | _ -> 0.

  let artillery = function
    | Ballista | Cyclops -> 2.
    | Mangonel -> 3.
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
    | Marms -> 0.003
    | Cavalry | Harpy -> 0.002
    | _ -> 0.

  let hit_chance = function
    | Ballista | Mangonel -> 0.25
    | Dervish | Harcher | Ranger | Xbowman -> 0.5
    | _ -> 1.

  let power = function
    | Dullahan -> 7.
    | Cyclops -> 5.
    | Harpy | Knight -> 4.
    | Marms | Templar -> 3.
    | Ballista | Berserker | Cavalry | Demon | Dervish | Harcher | Mangonel | Merc | Ranger | Veteran | Xbowman -> 2.
    | Men | Novice | Orc -> 1.
    | Skeleton -> 0.5

  let revive = function
    | Dervish -> 1.
    | Novice -> 0.5
    | _ -> 0.

  let supply_cost = function
    | Berserker -> 0
    | Cavalry
    | Ranger
    | Veteran -> 2
    | Marms
    | Merc -> 3
    | Templar -> 4
    | Knight -> 5
    | Ballista -> 12
    | Mangonel -> 16
    | _ -> 1

  let upkeep_cost = function
    | Knight -> 3
    | Ballista | Mangonel | Marms | Merc -> 2
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

let discard attr = Mapx.discardk (Attr.is attr)
let filter attr = Mapx.filterk (Attr.is attr)
let filterset = filter

module Promote = struct
  let needs = function
    | Harcher
    | Marms -> Cavalry
    | Knight -> Marms
    | Dervish -> Novice
    | Xbowman -> Veteran
    | _ -> Men

  let amount = function
    | Merc -> 0
    | Ballista
    | Berserker
    | Mangonel -> 2
    | _ -> 1

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

let has = Map.mem
let has_any attr = Mapx.existsk (Attr.is attr)

let is_empty t = Mapx.existsv ((<) 0) t |> not

let kinds_of t =
  let f k _ s = Set.add k s in
  Map.fold f t Set.empty

let power_of t =
  Map.mapi (fun k c -> float c *. Base.power k) t
  |> Mapx.Float.sum

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
  Map.partition (fun k _ -> Attr.is attr k) t

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
