type t
type manpower = int
type supply = int
type kind =
  | Empty
  | Manpwr of manpower
  | Supply of supply

val empty : t

val has_manp : t -> bool
val of_manp : manpower -> t
val manp_of : t -> manpower

val has_supp : t -> bool
val of_supp : supply -> t
val supp_of : t -> supply
val mis_supp : t -> t
val clr_supp : t -> t

val cost_of : t -> t
val can_afford : t -> t -> bool

val (<+) : t -> kind -> t
val (<~) : t -> kind -> t
val (++) : t -> t -> t
val (--) : t -> t -> t
