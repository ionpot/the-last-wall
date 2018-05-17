open Game_defs

type resource = Game_resource.t

val blessing : deity -> resource
val starting : deity -> resource
val support : unit -> resource option
val upkeep : manpower -> resource
