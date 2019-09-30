open Defs

type cost = Resource.t
type kind = Arena | Engrs | Fort | Foundry | Guesthouse | Market | Mausoleum of Leader.t | Observatory | Sawmill | Stable | Tavern | Temple | Trade of Nation.kind option
type bonus = To of kind | ToAll

val trade_default : kind

module Avlb : sig
  module Set : Set.S with type elt = kind
  type t = Set.t
end

module Bonus : sig
  type t = bonus * Resource.Bonus.t
end

module Built : sig
  type t = kind list
end

module Queue : sig
  type t = (kind * cost) list
end

module Ready : sig
  module Map : Map.S with type key = kind
  type t = count Map.t
end

type status = Built.t * Built.t * Queue.t

type t

val empty : t

val cost_of : kind -> Bonus.t list -> cost
val is_multiple : kind -> bool
val manpwr_range : kind -> manpower range
val supply_range : kind -> supply range

val arena_cap : t -> count
val available : t -> Avlb.t
val ballista_cap : t -> count
val count : kind -> t -> count
val has_trade : Nation.kind -> t -> bool
val is_built : kind -> t -> bool
val is_ready : kind -> t -> bool
val mausoleums : t -> count
val need_manp : t -> manpower
val need_supp : t -> supply
val need_trade : t -> bool
val ready : t -> Ready.t
val stable_cap : t -> count
val status : t -> status
val temple_cap : t -> count

val died : Leader.t -> t -> t
val manp : manpower -> manpower -> t -> t
val raze : kind -> t -> t
val set_ready : kind -> t -> t
val set_ready_ls : kind list -> t -> t
val set_trade : Nation.kind option -> t -> t
val start : kind list -> Bonus.t list -> t -> t
val supp : supply -> supply -> t -> t
val update : status -> t -> t
