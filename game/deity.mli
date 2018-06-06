type t = Arnerula | Elanis | Lerota | Sekrefir | Sitera | None

val t_list : t list

val blessing_of : t -> Resource.t option
val starting : t -> Resource.t
