type t

val empty : t

val first : t -> string
val full : t -> string
val house : t -> string
val title : t -> string

module Roll : sig
  val from : string list -> string list -> string list -> t -> t
end
