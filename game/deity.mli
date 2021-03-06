type t = Arnerula | Elanis | Lerota | Sekrefir | Sitera

val empty : t
val list : t list

module Roll : Dice.S -> sig
  val blessing : t -> Resource.t
  val boosted : t -> Resource.t
  val starting : t -> Resource.t
end
