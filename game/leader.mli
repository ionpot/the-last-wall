open Defs

type t
type ltype = Aristocrat | Expert | Warrior
type level = int

val make : unit -> t
val lives : unit -> bool
val alive : t -> bool
val won : t -> unit
val died : t -> turn -> unit
val died_at : t -> turn
val can_lvup : t -> bool
val lvup : t -> t
val type_of : t -> ltype
val level_of : t -> level
