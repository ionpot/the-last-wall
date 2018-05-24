open Defs

type resource = Resource.t

val blessing : deity -> resource
val starting : deity -> resource
val support : unit -> resource option
val upkeep : manpower -> resource
