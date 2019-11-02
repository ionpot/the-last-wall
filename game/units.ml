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

let to_dr kind n =
  Float.times n (Base.dr kind)

let to_upkeep kind n =
  n * Base.upkeep_cost kind

module Power = struct
  type t = kind -> Defs.power

  let base = Base.power

  let boost kind p t =
    fun k -> Float.add_if (k = kind) p (t k)

  let can_hit t kind pwr =
    t kind < pwr +. 4.

  let ceil t kind =
    Float.ceil_by (t kind)

  let div t kind p =
    p /. t kind

  let heal t kind p =
    let pwr = t kind in
    Float.floor_by pwr p, mod_float p pwr

  let modulo t kind p =
    mod_float p (t kind)

  let mul t kind n =
    Float.times n (t kind)

  let to_count t kind p =
    truncate (div t kind p)

  let translate ki ko n t =
    mul t ki n |> to_count t ko
end

type t =
  { map : Defs.count Map.t
  ; power : Power.t
  }

let empty =
  { map = Map.empty
  ; power = Power.base
  }

let make n kind =
  { empty with map = Map.singleton kind n }

let promotion_cost = function
  | Ballista
  | Berserker -> make 2 Men
  | Cavalry -> make 1 Men
  | Knight -> make 1 Cavalry
  | Ranger
  | Templar -> make 1 Dervish
  | _ -> empty

let set_map t map =
  { t with map }

let set_power t power =
  { t with power }

let discard attr t =
  Map.filter (fun k _ -> not (attr k)) t.map
  |> set_map t

let filter attr t =
  Map.filter (fun k _ -> attr k) t.map
  |> set_map t

module Ops = struct
  let add t_a t_b =
    Map.union (fun _ a b -> Some (a + b)) t_a.map t_b.map
    |> set_map t_b

  let div t_a t_b =
    let f _ a_opt = function
      | Some b ->
          if b > 0
          then Some (Number.maybe 0 a_opt / b)
          else None
      | None -> None
    in
    Map.merge f t_a.map t_b.map
    |> set_map t_b

  let is_empty t =
    Map.is_empty t.map

  let min t =
    let cmp n = function
      | Some x -> Some (min x n)
      | None -> Some n
    in
    Map.fold (fun _ -> cmp) t.map None
    |> Number.maybe 0

  let mul n t =
    Map.map (( * ) n) t.map
    |> set_map t

  let powers t =
    Map.mapi (Power.mul t.power) t.map

  let sub t_a t_b =
    let f _ a_opt = function
      | Some b -> Number.(sub_opt (maybe 0 a_opt) b)
      | None -> a_opt
    in
    Map.merge f t_a.map t_b.map
    |> set_map t_b

  let sum t =
    Map.fold (fun _ -> (+)) t.map 0

  let sumf m =
    Map.fold (fun _ -> (+.)) m 0.
end

let affordable kind cap t =
  let u = Ops.div t (promotion_cost kind) in
  if Ops.is_empty u then cap
  else Ops.min u |> min cap

let cost n kind =
  promotion_cost kind |> Ops.mul n

let count kind t =
  if Map.mem kind t.map
  then Map.find kind t.map
  else 0

let count_all = Ops.sum

let dr t =
  Map.mapi to_dr t.map
  |> Ops.sumf

let filter_count attr t =
  filter attr t |> count_all

let find n kind t =
  let found = count kind t in
  min n found

let has kind t =
  count kind t > 0

let is_empty = Ops.is_empty

let kinds_of t =
  let f k _ s = Set.add k s in
  Map.fold f t.map Set.empty

let max_base t =
  Map.fold (fun k _ acc -> t.power k |> max acc) t.map 0.

let power t =
  Ops.(powers t |> sumf)

let filter_power attr t =
  filter attr t |> power

let barrage_power t =
  let power = filter_power Attr.can_barrage t in
  let bonus = Float.times (count Ranger t) 1. in
  power +. bonus

let power_of kind t =
  Power.mul t.power kind (count kind t)

let promotable kind t =
  let u = Ops.div t (promotion_cost kind) in
  if Ops.is_empty u then 0
  else Ops.min u

let ratio_of kind t =
  let n = count kind t in
  let sum = Ops.sum t in
  Float.div (float n) (float sum)

let report t =
  Map.bindings t.map

let untouchable t_atk t_dfn =
  let pwr = max_base t_atk in
  let f k _ s = if Power.can_hit t_dfn.power k pwr then s else Set.add k s in
  Map.fold f t_dfn.map Set.empty

let upkeep t =
  Map.mapi to_upkeep t.map |> set_map t |> count_all

let add n kind t =
  Map.add kind (n + count kind t) t.map
  |> set_map t

let boost kind p t =
  Power.boost kind p t.power
  |> set_power t

let combine = Ops.add

let only kind t =
  let n = count kind t in
  (if n > 0 then Map.singleton kind n else Map.empty)
  |> set_map t

let pop kind t =
  let x, rest = Map.partition (fun k _ -> k = kind) t.map in
  set_map t x, set_map t rest

let reduce t t' =
  Ops.sub t' t

let split attr t =
  let a, b = Map.partition (fun k _ -> attr k) t.map in
  set_map t a, set_map t b

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
  Map.update kind (function Some x -> Number.sub_opt x n | x -> x) t.map
  |> set_map t

let pick_w m n =
  let key, _ = Map.min_binding m in
  let f k hit (k', n') =
    if n' > 0. then k, n' -. hit else k', n'
  in
  Map.fold f m (key, n) |> fst

module Dist = struct
  let threshold = 4.

  type acc =
    { absorbed : Defs.power
    ; healed : Defs.power
    ; power : Power.t
    ; reflected : Defs.power
    ; untouchable : Set.t
    }

  let to_t a m =
    { map = m
    ; power = a.power
    }

  let ceil_count a m =
    Map.mapi (Power.ceil a.power) m
    |> Map.mapi (Power.to_count a.power)
    |> to_t a

  type result = acc * Defs.power Map.t * Defs.power Map.t

  let empty_acc =
    { absorbed = 0.
    ; healed = 0.
    ; power = Power.base
    ; reflected = 0.
    ; untouchable = Set.empty
    }

  let empty : result = empty_acc, Map.empty, Map.empty

  let absorbed (a, _, m) =
    Map.mapi (Power.modulo a.power) m |> Ops.sumf |> (+.) a.absorbed
  let healed (a, _, _) = a.healed
  let no_remaining (_, m, _) = Map.is_empty m
  let outcome (a, _, m) = Map.mapi (Power.to_count a.power) m |> to_t a
  let reflected (a, _, _) = a.reflected
  let remaining (a, m, _) = ceil_count a m

  let absorb kind n acc =
    let n', healed = Power.heal acc.power kind n in
    { acc with absorbed = acc.absorbed +. healed }, n'

  let heal kind n acc =
    let n', healed = Power.heal acc.power kind n in
    { acc with healed = acc.healed +. healed }, n'

  let reflect kind n acc =
    let n' = Power.modulo acc.power kind n in
    { acc with reflected = acc.reflected +. n' }, n

  let handle kind acc dmg =
    let acc', sub =
      if Set.mem kind acc.untouchable
      then absorb kind dmg acc
      else if Attr.can_heal kind
      then heal kind dmg acc
      else if Attr.can_reflect kind
      then reflect kind dmg acc
      else acc, dmg
    in acc', dmg, sub

  let mitigate kind input acc cap =
    let ratio = ceil_count acc input |> ratio_of kind in
    max (ratio *. cap) 0.1

  let pick kind input acc cap =
    (if cap > threshold then mitigate kind input acc cap else cap)
    |> min (Map.find kind input)

  module Roll (Dice : Dice.S) = struct
    let ratio cap =
      if cap > threshold
      then Dice.ratio threshold *. cap
      else cap

    module Pick = Pick.WithAcc(struct
      module Cap = Pick.Float
      module Map = Map
      module Type = Cap
      type nonrec acc = acc
      type map = Type.t Map.t
      type step = acc * Cap.t * Type.t
      let choose input =
        let probs = Map.mapi (fun k _ -> Base.hit_chance k) input in
        Ops.sumf probs |> Dice.rollf |> pick_w probs
      let roll acc kind cap input =
        ratio cap |> pick kind input acc |> handle kind acc
    end)

    let from cap t' t =
      let untouchable = untouchable t' t in
      let acc = { empty_acc with power = t.power; untouchable } in
      let input = Ops.powers t in
      let output = Map.empty in
      Pick.from acc cap input output
  end
end

let random_key roll t =
  let n = Map.cardinal t |> roll in
  let key, _ = Map.choose t in
  let f k _ (k', n') =
    if n' > 0 then k, pred n' else k', n'
  in
  Map.fold f t (key, n) |> fst

module Fill (Dice : Dice.S) = struct
  module Pick = Pick.WithAcc(struct
    module Cap = Pick.Float
    module Map = Map
    module Type = Pick.Int
    type acc = Power.t
    type map = Type.t Map.t
    type step = acc * Cap.t * Type.t
    let choose = random_key Dice.roll
    let roll power kind cap t =
      let n = Map.find kind t |> min (Power.to_count power kind cap) |> Dice.roll in
      power, Power.mul power kind n, n
  end)

  let to_t power (_, _, map) =
    { map; power }

  let from cap t =
    if cap > power t then t
    else Pick.from t.power cap t.map Map.empty |> to_t t.power
end

module FillCount (Dice : Dice.S) = struct
  module Pick = Pick.With(struct
    module Cap = Pick.Int
    module Map = Map
    module Type = Cap
    type map = Type.t Map.t
    type step = Cap.t * Type.t
    let choose = random_key Dice.roll
    let roll kind cap t =
      let n = Map.find kind t |> min cap |> Dice.roll in
      n, n
  end)

  let from total t =
    if total > count_all t then t
    else Pick.from total t.map Map.empty |> set_map t
end

module Report (Dice : Dice.S) = struct
  let try_round x =
    if x > 10 then 10 * Dice.round (0.1 *. float x) else x

  let from t =
    Map.map try_round t.map
    |> set_map t
    |> report

  let sum_from t =
    try_round (count_all t), (kinds_of t)
end
