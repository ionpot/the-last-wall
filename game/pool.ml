type kind = Arena | Novice
type value = Defs.count

module Kind = struct
  type t = kind
  let compare = compare
end
module Map = Map.Make(Kind)
module Mapx = Mapx.Make(Map)

type t = Defs.count Map.t

let empty = Map.empty

let get = Mapx.Int.value

let add = Mapx.Int.add_to
let set = Map.add
let sub = Mapx.Int.sub_from
