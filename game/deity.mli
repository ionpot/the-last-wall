type t = Elanis | Sekrefir | Sitera | None

val t_list : t list

val blessing_of : t -> Resource.t
val starting : t -> Resource.t
