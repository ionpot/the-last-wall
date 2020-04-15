type t = Arnerula | Elanis | Lerota | Sekrefir | Sitera

module Set : Set.S with type elt = t

val empty : t
val set : Set.t

module Roll : sig
  val blessing : t -> Resource.t
  val boosted : t -> Resource.t
  val starting : t -> Resource.t
end
