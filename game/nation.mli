type t = Tulron | Sodistan | Hekatium | Numendor | Clan
type resource = Resource.t
type support = (t * resource option)

val t_list : t list

val pickN : int -> t list -> t list
val support_of : t -> support
val support_of_list : t list -> support list
val total_of : support list -> resource
val apply_bonus : support list -> resource -> support list
