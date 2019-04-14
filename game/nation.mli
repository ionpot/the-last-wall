type kind = Tulron | Sodistan | Hekatium | Numendor | Clan
type support = (kind * Resource.t)
type t

val empty : t
val kinds : kind list
val max_allowed : int

val from : kind list -> t

val add : Resource.t -> support list -> support list
val sum : support list -> Resource.t
val support : t -> support list
val which : t -> kind list
