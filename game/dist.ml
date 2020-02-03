module Attr = Units.Attr
module Map = Power.Map
module Mapx = Mapx.Make(Map)
module Set = Units.Set

let threshold = 4.

type acc =
  { absorbed : Defs.power
  ; base : Power.t
  ; healed : Defs.power
  ; ratios : Power.t
  ; reflected : Defs.power
  ; retreated : Power.t
  ; untouchable : Set.t
  }

let empty_acc =
  { absorbed = 0.
  ; base = Power.base
  ; healed = 0.
  ; ratios = Power.base
  ; reflected = 0.
  ; retreated = Power.(set_attr Attr.hit_run 0. base)
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
let retreated (a, _, _) = Power.count a.base a.retreated

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
    { acc with absorbed = acc.absorbed +. healed }, (p', p')

  let heal kind p acc =
    let p', healed = Power.heal kind p acc.base in
    { acc with healed = acc.healed +. healed }, (p', p')

  let reflect kind p acc =
    let p' = Power.Fn.modulo acc.base kind p in
    { acc with reflected = acc.reflected +. p' }, (p, p)

  let retreat kind p acc =
    let p' = Power.Fn.modulo acc.base kind p in
    let retreated = Power.add kind p' acc.retreated in
    { acc with retreated }, (p, 0.)

  let handle kind acc dmg =
    let acc', sub =
      if Set.mem kind acc.untouchable
      then absorb kind dmg acc
      else if Attr.(is heal) kind
      then heal kind dmg acc
      else if Attr.(is reflect) kind
      then reflect kind dmg acc
      else if Attr.(is hit_run) kind
      then retreat kind dmg acc
      else acc, (dmg, dmg)
    in acc', dmg, sub

  let mitigate kind _ acc cap =
    let ratio = Map.find kind acc.ratios in
    max (ratio *. cap) 0.1

  let picked kind input acc cap =
    (if cap > threshold then mitigate kind input acc cap else cap)
    |> min (Map.find kind input)

  let choose ratios input =
    if Flags.use_ratio
    then
      let hit_chance k ratio = ratio *. Units.Base.hit_chance k in
      let probs = Map.mapi hit_chance ratios in
      Power.sum probs |> Dice.rollf |> Power.pick probs
    else
      let module Roll = Dice.Map(Map) in
      Roll.key input

  module Pick = Pick.WithAcc(struct
    module Cap = Pick.Float
    module Map = Map
    module Type = Cap
    type nonrec acc = acc
    type map = Type.t Map.t
    type step = acc * Cap.t * (Type.t * Type.t)
    let choose acc _ input =
      let units = to_units acc input in
      let f k = Units.ratio_of k units in
      let ratios = Mapx.mapk f units in
      let chosen = choose ratios input in
      { acc with ratios }, Some chosen
    let roll acc cap kind input =
      ratio cap |> picked kind input acc |> handle kind acc
  end)

  let from cap base atk dfn =
    let untouchable = Power.untouchable atk dfn base in
    let acc = { empty_acc with base; untouchable } in
    let input = Power.from_units dfn base in
    let output = Power.base in
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
    type step = acc * Cap.t * (Type.t * Type.t)
    let choose base cap input =
      let p = Power.map_units input base |> Power.min in
      base, if p > cap then None else Some (Roll.key input)
    let roll base cap kind t =
      let p = Map.find kind t
        |> min (Power.Fn.count base kind cap)
        |> Dice.roll
      in
      base, Power.Fn.mul base kind p, (p, p)
  end)

  let from cap base units =
    if cap > Power.of_units units base
    then units, Units.empty
    else
      let _, rem, picked = Pick.from base cap units Units.empty in
      picked, rem
end

let fill ~base ~order power units =
  let f (p, picked) kind =
    let n = Power.Fn.count base kind p in
    let count = Units.count kind units in
    let n', count' = Number.take n count in
    Power.Fn.mul base kind n',
    Units.add count' kind picked
  in
  List.fold_left f (power, Units.empty) order
