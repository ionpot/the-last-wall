type t = Tulron | Sodistan | Hekatium | Numendor | Clan
type support = (t * Resource.t option)

val t_list : t list

val filter : t list -> t list
val support_of : t -> support
val support_of_list : t list -> support list
val total_of : support list -> Resource.t
val apply_bonus : Resource.t -> support list -> support list
