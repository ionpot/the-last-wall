open Defs

type t
type state
type ltype = Aristocrat | Expert | Warrior
type level = int
type loss = Resource.t
type charisma = int

val empty : state
val dead : state

val of_state : state -> t option
val state_of : t -> state
val need : state -> bool
val tick : state -> state

val make : unit -> t
val lives : unit -> bool
val won : t -> unit
val can_lvup : t -> bool
val lvup : t -> t
val type_of : t -> ltype
val level_of : t -> level
val cha_of : t -> charisma
val mitigate : loss -> t -> loss
val res_bonus_of : t -> Resource.t
