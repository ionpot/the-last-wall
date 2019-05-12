open Defs

type cost = Resource.t
type kind = Engrs | Fort | Market | Mausoleum of Leader.t | Observatory | Stable | Tavern | Temple | Trade of Nation.trade
type queued = kind * cost
type status = kind list * kind list * queued list
type t

val empty : t

val multiple : kind -> bool

val built : kind -> t -> bool
val cost_of : kind -> t -> cost
val count : kind -> t -> count
val ls_avlb : t -> kind list
val ls_built : t -> kind list
val ls_ready : t -> kind list
val mausoleums : t -> count
val need_manp : t -> manpower
val need_supp : t -> supply
val ready : kind -> t -> bool
val status : t -> status
val trade : t -> Nation.trade

val died : Leader.t -> t -> t
val manp : manpower -> t -> t
val raze : kind -> t -> t
val set_trade : Nation.trade -> t -> t
val start : kind list -> t -> t
val supp : supply -> t -> t
val update : status -> t -> t
