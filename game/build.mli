open Defs

type kind = Arena | Barracks | Engrs | Fort | Foundry | Guesthouse | Market | Mausoleum of Leader.t | Observatory | Sawmill | Stable | Tavern | Temple | Trade
type cost = Resource.t

val base_cost : kind -> cost
val is_multiple : kind -> bool
val manpwr_range : kind -> manpower range
val supply_range : kind -> supply range

module Map : Map.S with type key = kind
module Set : Set.S with type elt = kind
module Queue : module type of Queue.Make(Set)

type cost_map = cost Map.t

module Avlb : sig
  type t = Set.t
end

module Built : sig
  type t = kind list
end

module Ready : sig
  type t = count Map.t
end

type status = Built.t * Built.t * Queue.t

type t

val empty : t

val available : t -> Avlb.t
val built : t -> Built.t
val cap_of : kind -> t -> count
val cost_map : t -> cost_map
val count : kind -> t -> count
val is_built : kind -> t -> bool
val is_complete : kind -> t -> bool
val is_ready : kind -> t -> bool
val mausoleums : t -> count
val needs : t -> Resource.t
val queue : t -> Queue.t
val ready : t -> Ready.t
val status : t -> status

val apply_mnp : manpower -> t -> t
val apply_sup : supply -> t -> supply * t
val died : Leader.t -> t -> t
val raze : kind -> t -> t
val set_ready : kind -> t -> t
val set_ready_ls : kind list -> t -> t
val start : kind -> cost_map -> t -> t
val update : status -> t -> t
