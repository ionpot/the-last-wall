open Defs

type t
type kind =
  | Empty
  | Manpwr of manpower
  | Supply of supply

val empty : t

val has_manp : t -> bool
val of_manp : manpower -> t
val manp_of : t -> manpower
val set_manp : t -> manpower -> t

val has_supp : t -> bool
val of_supp : supply -> t
val supp_of : t -> supply
val set_supp : t -> supply -> t
val mis_supp : t -> t
val clr_supp : t -> t

val deduce_manp : t -> t -> t * t
val deduce_supp : t -> t -> t * t

val manp2supp : t -> t
val supp2manp : t -> t

val (<+) : t -> kind -> t
val (<~) : t -> kind -> t
val (++) : t -> t -> t
val (--) : t -> t -> t
