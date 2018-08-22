type mercs = Resource.t

val roll : unit -> mercs option
val buy : mercs -> Resource.t -> Resource.t
