type kind = Arena | Marms | Novice
type value = Defs.count

type t

val empty : t

val get : kind -> t -> value

val add : kind -> value -> t -> t
val set : kind -> value -> t -> t
val sub : kind -> value -> t -> t
