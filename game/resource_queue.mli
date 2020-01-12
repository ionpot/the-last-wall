module FromSet (Set : Set.S) : sig
  type t

  val empty : t

  val cost : t -> Resource.t
  val to_set : t -> Set.t

  val add : Set.elt -> Resource.t -> t -> t
  val partition : Resource.t -> t -> Resource.t * t * t
  val rm_set : Set.t -> t -> t
end
