module Attr = Units.Attr
module Map = Power.Map
module Mapx = Mapx.Make(Map)
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

let to_units acc map =
  Power.ceil_count acc.base map

let absorbed (a, _, m) =
  Power.(modulo a.base m |> sum) |> (+.) a.absorbed
let healed (a, _, _) = a.healed
let no_remaining (_, m, _) = Map.is_empty m
let outcome (a, _, m) = Power.count a.base m
let reflected (a, _, _) = a.reflected
let remaining (a, m, _) = to_units a m

module type Flags = sig
  val full_absorb : bool
  val use_ratio : bool
end

module Damage (Dice : Dice.S) (Flags : Flags) = struct
  let ratio cap =
    if cap > threshold
    then Dice.ratio threshold *. cap
    else cap

  let absorb kind p acc =
    let p', healed =
      if Flags.full_absorb then 0., p
      else Power.heal kind p acc.base
    in
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

  let mitigate kind input acc cap =
    let ratio = to_units acc input |> Units.ratio_of kind in
    max (ratio *. cap) 0.1

  let picked kind input acc cap =
    (if cap > threshold then mitigate kind input acc cap else cap)
    |> min (Map.find kind input)

  module Pick = Pick.WithAcc(struct
    module Cap = Pick.Float
    module Map = Map
    module Type = Cap
    type nonrec acc = acc
    type map = Type.t Map.t
    type step = acc * Cap.t * Type.t
    let choose acc cap input =
      if Flags.use_ratio
      then
        let units = to_units acc input in
        let hit_chance k =
          Units.ratio_of k units *. Units.Base.hit_chance k
        in
        let probs = Mapx.mapk hit_chance input in
        Some (Power.sum probs |> Dice.rollf |> Mapx.Float.pick probs)
      else
        let module Roll = Dice.Map(Map) in
        Some (Roll.key input)
    let roll acc cap kind input =
      ratio cap |> picked kind input acc |> handle kind acc
  end)

  let from cap base atk dfn =
    let untouchable = Power.untouchable atk dfn base in
    let acc = { empty_acc with base; untouchable } in
    let input = Power.from_units dfn base in
    let output = Power.empty in
    Pick.from acc cap input output
end

module Fill (Dice : Dice.S) = struct
  module Roll = Dice.Map(Map)
  module Pick = Pick.WithAcc(struct
    module Cap = Pick.Float
    module Map = Map
    module Type = Pick.Int
    type acc = Power.t
    type map = Type.t Map.t
    type step = acc * Cap.t * Type.t
    let choose base cap input =
      let p = Power.map_units input base |> Power.min in
      if p > cap then None else Some (Roll.key input)
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
