type kind = BlackArmy

module Cost : sig
  val men : kind -> Defs.manpower
  val resource : kind -> Resource.t
  val supply : kind -> Defs.supply
end

module Map : Map.S with type key = kind
module Set : Set.S with type elt = kind

module type Progress = sig
  val cancelled : Set.t
  val men_used : Defs.manpower
  val rem_supply : Defs.supply
  val started : Set.t
end

type t

val empty : t

val apply : Defs.manpower -> Defs.supply -> t -> (module Progress)
val available : t -> Set.t
val complete : t -> Set.t
val is_complete : kind -> t -> bool

val set_complete : Set.t -> t -> t
val set_progress : (module Progress) -> t -> t
val start : kind list -> t -> t
val tick : t -> t
val unlock : kind -> bool -> t -> t
