module Attr = Units.Attr
module Map = Power.Map
module Set = Units.Set

let threshold = 4.

type acc =
  { absorbed : Defs.power
  ; base : Power.t
  ; healed : Defs.power
  ; reflected : Defs.power
  ; untouchable : Set.t
  }

let empty_acc =
  { absorbed = 0.
  ; base = Power.empty
  ; healed = 0.
  ; reflected = 0.
  ; untouchable = Set.empty
  }

type t = acc * Power.t * Power.t

let empty : t = empty_acc, Map.empty, Map.empty

let absorbed (a, _, m) =
  Power.(modulo a.base m |> sum) |> (+.) a.absorbed
let healed (a, _, _) = a.healed
let no_remaining (_, m, _) = Map.is_empty m
let outcome (a, _, m) = Power.count a.base m
let reflected (a, _, _) = a.reflected
let remaining (a, m, _) = Power.ceil_count a.base m

let absorb kind p acc =
  let p', healed = Power.heal kind p acc.base in
  { acc with absorbed = acc.absorbed +. healed }, p'

let heal kind p acc =
  let p', healed = Power.heal kind p acc.base in
  { acc with healed = acc.healed +. healed }, p'

let reflect kind p acc =
  let p' = Power.Fn.modulo acc.base kind p in
  { acc with reflected = acc.reflected +. p' }, p

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

let unit2str = function
  | Ballista -> "ballista"
  | Berserker -> "berserker"
  | Cavalry -> "cavalry"
  | Cyclops -> "cyclops"
  | Demon -> "demon"
  | Dervish -> "dervish"
  | Harpy -> "harpy"
  | Knight -> "knight"
  | Men -> "men"
  | Merc -> "merc"
  | Orc -> "orc"
  | Ranger -> "ranger"
  | Skeleton -> "skeleton"
  | Templar -> "templar"

let mitigate kind input acc cap =
  let ratio = Power.ceil_count acc.base input |> Units.ratio_of kind in
  Printf.printf " -> %.3f ratio" ratio;
  max (ratio *. cap) 0.1

let picked kind input acc cap =
  (if cap > threshold then mitigate kind input acc cap else cap)
  |> min (Map.find kind input)

let pick m p =
  let key, _ = Map.min_binding m in
  let f k hit (k', p') =
    if p' > 0. then k, p' -. hit else k', p'
  in
  Map.fold f m (key, p) |> fst

module Damage (Dice : Dice.S) = struct
  let ratio cap =
    if cap > threshold
    then (Dice.ratio threshold *. cap)
      |> (fun r -> Printf.printf " -> %.3f divided" r;r)
    else cap

  module Pick = Pick.WithAcc(struct
    module Cap = Pick.Float
    module Map = Map
    module Type = Cap
    type nonrec acc = acc
    type map = Type.t Map.t
    type step = acc * Cap.t * Type.t
    let choose acc cap input =
      let f k _ = Units.Base.hit_chance k in
      let probs = Map.mapi f input in
      Some (Power.sum probs |> Dice.rollf |> pick probs)
    let roll acc cap kind input =
      Printf.printf "%.3f cap" cap;
      ratio cap |> picked kind input acc |> handle kind acc
      |> (fun (a, d, s) -> Printf.printf " -> %.3f %s\n" d (unit2str kind);a,d,s)
  end)

  let from cap base atk dfn =
    let untouchable = Power.untouchable atk dfn base in
    let acc = { empty_acc with base; untouchable } in
    let input = Power.from_units dfn base in
    let output = Power.empty in
    Pick.from acc cap input output
end

module Fill (Dice : Dice.S) = struct
  module Roll = Units.Roll(Dice)
  module Pick = Pick.WithAcc(struct
    module Cap = Pick.Float
    module Map = Map
    module Type = Pick.Int
    type acc = Power.t
    type map = Type.t Map.t
    type step = acc * Cap.t * Type.t
    let choose base cap input =
      let p = Power.map_units input base |> Power.min in
      if p > cap then None else Some (Roll.kind input)
    let roll base cap kind t =
      let p = Map.find kind t
        |> min (Power.Fn.count base kind cap)
        |> Dice.roll
      in
      base, Power.Fn.mul base kind p, p
  end)

  let from cap base units =
    if cap > Power.of_units units base
    then units, Units.empty
    else
      let _, rem, picked = Pick.from base cap units Units.empty in
      picked, rem
end
