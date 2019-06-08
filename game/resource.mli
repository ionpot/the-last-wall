open Defs

module Bonus : sig
  type kind = Mnp of float | Sup of float | Both of float
  type t = Add of kind | Sub of kind
end

type t
type kind =
  | Empty
  | Manpwr of manpower
  | Supply of supply

val empty : t

val bonus_to : t -> Bonus.t -> t

val has_manp : t -> bool
val of_manp : manpower -> t
val manp_of : t -> manpower
val set_manp : t -> manpower -> t
val map_manp : (manpower -> manpower) -> t -> t

val has_supp : t -> bool
val of_supp : supply -> t
val supp_of : t -> supply
val set_supp : t -> supply -> t
val mis_supp : t -> t
val clr_supp : t -> t
val map_supp : (supply -> supply) -> t -> t

val deduce_manp : manpower -> t -> manpower * t
val deduce_supp : supply -> t -> supply * t

val take_manp : manpower -> t -> manpower * t
val take_supp : supply -> t -> supply * t

val manp2supp : t -> t
val supp2manp : t -> t

val (<+) : t -> kind -> t
val (<~) : t -> kind -> t
val (++) : t -> t -> t
val (--) : t -> t -> t
