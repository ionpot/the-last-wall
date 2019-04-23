open Defs

type cost = Resource.t
type kind = Fort | Market | Mausoleum of Leader.t | Observatory | Stable | Tavern | Temple
type queued = kind * cost
type status = kind list * kind list * queued list
type t

val empty : t

val cost_of : kind -> cost
val multiple : kind -> bool

val available : t -> kind list
val count : kind -> t -> count
val mausoleums : t -> count
val need_manp : t -> manpower
val need_supp : t -> supply
val present : kind -> t -> bool
val ready : kind -> t -> bool
val status : t -> status

val died : Leader.t -> t -> t
val manp : manpower -> t -> t
val raze : kind -> t -> t
val start : kind list -> t -> t
val supp : supply -> t -> t
val update : status -> t -> t
