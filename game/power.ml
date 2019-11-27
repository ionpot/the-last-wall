module Map = Units.Map
module Mapx = Mapx.Make(Map)
module Set = Units.Set

type t = Defs.power Map.t

let empty : t = Map.empty

let dr u =
  let f k n = Float.times n (Units.Base.dr k) in
  Map.mapi f u

let revive u =
  Mapx.mapk Units.Base.revive u

module Fn = struct
  let find k t =
    if Map.mem k t then Map.find k t else Units.Base.power k

  let can_hit t kind p =
    find kind t < p +. 4.

  let ceil t kind =
    Float.ceil_by (find kind t)

  let div t kind p =
    p /. find kind t

  let count t kind p =
    truncate (div t kind p)

  let modulo t kind p =
    mod_float p (find kind t)

  let mul t kind n =
    Float.times n (find kind t)
end

let from_units u t =
  Map.mapi (Fn.mul t) u

let count base t =
  Map.mapi (Fn.count base) t

let heal kind p t =
  let pwr = Fn.find kind t in
  Float.floor_by pwr p, mod_float p pwr

let min = Mapx.min
let max = Mapx.Float.max
let pick = Mapx.Float.pick
let sum = Mapx.Float.sum

let of_unit kind u t =
  Fn.mul t kind (Units.count kind u)

let of_units u t =
  from_units u t |> sum

let translate ki ko n t =
  Fn.mul t ki n |> Fn.count t ko

let add kind pwr t =
  let p = Fn.find kind t +. pwr in
  Map.add kind p t

let ceil base t =
  Map.mapi (Fn.ceil base) t

let ceil_count base t =
  ceil base t |> count base

let map_units u t =
  Mapx.mapk (fun k -> Fn.find k t) u

let modulo base t =
  Map.mapi (Fn.modulo base) t

let sub kind pwr t =
  let p = Float.sub (Fn.find kind t) pwr in
  Map.add kind p t

let barrage =
  add Units.Ranger 1. empty

let base bonus =
  let apply b k p t =
    if Bonus.has b bonus then add k p t else t
  in
  empty
  |> apply Bonus.Barrage Units.Harpy ~-.1.
  |> apply Bonus.ClanTrade Units.Ballista 1.

let untouchable atk dfn t =
  let pwr = map_units atk t |> max in
  let f k _ s = if Fn.can_hit t k pwr then s else Set.add k s in
  Map.fold f dfn Set.empty

module Roll (Dice : Dice.S) = struct
  let fear units base =
    let a = float (Units.count_all units) in
    let b = of_units units base in
    Dice.betweenf a b
end
