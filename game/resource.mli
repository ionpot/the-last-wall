open Defs

module Bonus : sig
  type kind = Mnp of float | Sup of float | Both of float
  type t = Add of kind | Sub of kind
end

type t

val empty : t

val make : ?mnp:manpower -> ?sup:supply -> unit -> t

val has : t -> t -> bool
val has_mnp : t -> bool
val has_sup : t -> bool
val mnp : t -> manpower
val sup : t -> supply

val add : ?mnp:manpower -> ?sup:supply -> t -> t
val bonus : Bonus.t -> t -> t
val bonus_if : bool -> Bonus.t -> t -> t
val deduce : t -> t -> t * t

val (++) : t -> t -> t
val (--) : t -> t -> t
