type t
type ltype = Aristocrat | Expert | Warrior
type level = int

val make : unit -> t
val lives : unit -> bool
val won : t -> unit
val type_of : t -> ltype
val level_of : t -> level
