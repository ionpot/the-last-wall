open Defs

type cost = Resource.t
type kind = Engrs | Fort | Market | Mausoleum of Leader.t | Observatory | Stable | Tavern | Temple | Trade of Nation.trade
type bonus = To of kind | ToAll
type queued = kind * cost
type status = kind list * kind list * queued list

module Bonus : sig
  type t = bonus * Resource.Bonus.t
end

type t

val empty : t

val cost_of : kind -> Bonus.t list -> cost
val multiple : kind -> bool

val built : kind -> t -> bool
val count : kind -> t -> count
val ls_avlb : t -> kind list
val ls_built : t -> kind list
val ls_queue : t -> queued list
val ls_ready : t -> kind list
val mausoleums : t -> count
val need_manp : t -> manpower
val need_supp : t -> supply
val ready : kind -> t -> bool
val status : t -> status
val trade : t -> Nation.trade

val died : Leader.t -> t -> t
val manp : manpower -> manpower -> t -> t
val raze : kind -> t -> t
val set_trade : Nation.trade -> t -> t
val start : kind list -> Bonus.t list -> t -> t
val supp : supply -> supply -> t -> t
val update : status -> t -> t
