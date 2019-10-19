type kind = Ballista | Berserker | Cavalry | Cyclops | Demon | Dervish | Harpy | Knight | Men | Merc | Orc | Ranger | Skeleton | Templar
type report = (kind * Defs.count) list
type sum_report = (Defs.count * kind list)

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

let can_hit kind pwr =
  Base.power kind < pwr +. 4.

let to_dr kind n =
  Float.times n (Base.dr kind)

let from_powerf kind p =
  p /. (Base.power kind)

let from_power kind p =
  from_powerf kind p
  |> truncate

let to_power kind n =
  Float.times n (Base.power kind)

let from_upkeep kind sup =
  Number.div sup (Base.upkeep_cost kind)

let to_upkeep kind n =
  n * Base.upkeep_cost kind

let ceil_power kind =
  Base.power kind |> Float.ceil_by

let mod_power kind n =
  Base.power kind |> mod_float n

let heal kind n =
  let pwr = Base.power kind in
  Float.floor_by pwr n, mod_float n pwr

let translate kind_in kind_out n =
  to_power kind_in n
  |> from_power kind_out

module Map = Map.Make(struct
  type t = kind
  let compare = compare
end)

type t = Defs.count Map.t

let empty : t = Map.empty

let make n kind =
  Map.singleton kind n

let promotion_cost = function
  | Ballista
  | Berserker -> make 2 Men
  | Cavalry -> make 1 Men
  | Knight -> make 1 Cavalry
  | Ranger
  | Templar -> make 1 Dervish
  | _ -> empty

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

  let powers t =
    Map.mapi to_power t

  let sub t_a t_b =
    let f _ a_opt = function
      | Some b -> Number.(sub_opt (maybe 0 a_opt) b)
      | None -> a_opt
    in
    Map.merge f t_a t_b

  let sum t =
    Map.fold (fun _ -> (+)) t 0

  let sumf t =
    Map.fold (fun _ -> (+.)) t 0.
end

let affordable kind cap t =
  let m = Ops.div t (promotion_cost kind) in
  if Map.is_empty m then cap
  else Ops.min m |> min cap

let cost n kind =
  promotion_cost kind |> Ops.mul n

let count kind t =
  try Map.find kind t with
  | Not_found -> 0

let count_all = Ops.sum

let dr t =
  Map.mapi to_dr t
  |> Ops.sumf

let filter_count attr t =
  filter attr t |> count_all

let find n kind t =
  let found = count kind t in
  min n found

let has kind t =
  count kind t > 0

let kinds_of t =
  Map.bindings t
  |> List.map fst

let power t =
  Ops.(powers t |> sumf)

let filter_power attr t =
  filter attr t |> power

let barrage_power t =
  let power = filter_power Attr.can_barrage t in
  let bonus = Float.times (count Ranger t) 1. in
  power +. bonus

let max_base_power t =
  Map.fold (fun k _ acc -> Base.power k |> max acc) t 0.

let power_of kind t =
  to_power kind (count kind t)

let promotable kind t =
  let m = Ops.div t (promotion_cost kind) in
  if Map.is_empty m then 0
  else Ops.min m

let ratio_of kind t =
  let n = count kind t in
  let sum = Ops.sum t in
  Float.div (float n) (float sum)

let report = Map.bindings

let untouchable t_atk t_dfn =
  let pwr = max_base_power t_atk in
  Map.fold (fun k _ ls -> if can_hit k pwr then ls else k :: ls) t_dfn []

let upkeep t =
  Map.mapi to_upkeep t |> count_all

let add n kind t =
  Map.add kind (n + count kind t) t

let combine = Ops.add

let only kind t =
  let n = count kind t in
  if n > 0 then Map.add kind n empty else empty

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

let pick_w t n =
  let key, _ = Map.min_binding t in
  let f k hit (k', n') =
    if n' > 0. then k, n' -. hit else k', n'
  in
  Map.fold f t (key, n) |> fst

module Dist = struct
  let threshold = 4.

  let ceil_count m =
    Map.(mapi ceil_power m |> mapi from_power)

  type acc =
    { absorbed : Defs.power
    ; healed : Defs.power
    ; reflected : Defs.power
    ; untouchable : kind list
    }
  type result = acc * Defs.power Map.t * Defs.power Map.t

  let empty_acc =
    { absorbed = 0.
    ; healed = 0.
    ; reflected = 0.
    ; untouchable = []
    }
  let empty : result = empty_acc, Map.empty, Map.empty

  let absorbed (a, _, m) =
    Map.mapi mod_power m |> Ops.sumf |> (+.) a.absorbed
  let healed (a, _, _) = a.healed
  let no_remaining (_, m, _) = Map.is_empty m
  let outcome (_, _, m) = Map.mapi from_power m
  let reflected (a, _, _) = a.reflected
  let remaining (_, m, _) = ceil_count m

  let absorb kind n acc =
    let n', healed = heal kind n in
    { acc with absorbed = acc.absorbed +. healed }, n'

  let heal kind n acc =
    let n', healed = heal kind n in
    { acc with healed = acc.healed +. healed }, n'

  let reflect kind n acc =
    let n' = mod_power kind n in
    { acc with reflected = acc.reflected +. n' }, n

  let handle kind acc dmg =
    let acc', sub =
      if List.mem kind acc.untouchable
      then absorb kind dmg acc
      else if Attr.can_heal kind
      then heal kind dmg acc
      else if Attr.can_reflect kind
      then reflect kind dmg acc
      else acc, dmg
    in acc', dmg, sub

  let mitigate kind input cap =
    let ratio = ceil_count input |> ratio_of kind in
    max (ratio *. cap) 0.1

  let pick kind input cap =
    (if cap > threshold then mitigate kind input cap else cap)
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
        ratio cap |> pick kind input |> handle kind acc
    end)

    let from cap untouchable t =
      let acc = { empty_acc with untouchable } in
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
  module Pick = Pick.With(struct
    module Cap = Pick.Float
    module Map = Map
    module Type = Pick.Int
    type map = Type.t Map.t
    type step = Cap.t * Type.t
    let choose = random_key Dice.roll
    let roll kind cap t =
      let n = Map.find kind t |> min (from_power kind cap) |> Dice.roll in
      to_power kind n, n
  end)

  let from pwr t =
    if pwr > power t then t
    else Pick.from pwr t empty
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
