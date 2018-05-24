open Defs

type t
type kind =
  | Empty
  | Manpwr of manpower
  | Supply of supply

val make : kind -> t
val manp : t -> manpower
val supp : t -> supply

val (<+) : t -> kind -> t
val (<~) : t -> kind -> t
val (++) : t -> t -> t
val (--) : t -> t -> t
