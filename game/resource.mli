open Defs

type t
type kind =
  | Empty
  | Manpwr of manpower
  | Supply of supply

val make : kind -> t
val empty : t -> bool
val manp : t -> manpower
val supp : t -> supply
val supp_missing : t -> supply

val (<+) : t -> kind -> t
val (<~) : t -> kind -> t
val (++) : t -> t -> t
val (--) : t -> t -> t
