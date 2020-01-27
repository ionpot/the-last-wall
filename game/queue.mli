module Make (Set : Set.S) : sig
  type t

  val empty : t

  val cost : t -> Resource.t
  val to_list : t -> (Set.elt * Resource.t) list
  val to_set : t -> Set.t

  val add : Set.elt -> Resource.t -> t -> t
  val apply : Resource.t -> t -> Resource.t * t
  val apply_if : (Resource.t -> bool) -> Resource.t -> t -> Resource.t * t
  val partition : Resource.t -> t -> Resource.t * t * t
  val pop_finished : t -> Set.elt list * t
  val rm_set : Set.t -> t -> t
end
