open Defs

type resource = Resource.t

val blessing : deity -> resource
val starting : deity -> resource
val upkeep : manpower -> resource
