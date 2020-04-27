type t

val apply : t -> State.t -> State.t
val make : State.t -> t

val chances : t -> Nation.chances
val resources : t -> Nation.resources

val chances_init : State.t -> Nation.chances
