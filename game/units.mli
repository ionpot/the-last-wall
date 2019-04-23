type kind = Cavalry | Demon | Dervish | Men | Orc | Skeleton
type report = (Defs.count * kind) list
type sum_report = (Defs.count * kind list)

val attacks : kind list
val defends : kind list

val abundance_of : kind -> float
val chance_of : kind -> float

type t

val empty : t

val make : Defs.count -> kind -> t

val count : kind -> t -> Defs.count
val count_all : t -> Defs.count
val find : Defs.count -> kind -> t -> Defs.count
val has : kind -> t -> bool
val kinds_of : t -> kind list
val power : t -> Defs.power
val power_of : kind -> t -> Defs.power
val ratio : kind -> kind -> t -> float
val starve : Defs.supply -> t -> t
val upkeep : t -> Defs.supply

val add : Defs.count -> kind -> t -> t
val sub : Defs.count -> kind -> t -> t

val combine : t -> t -> t
val reduce : t -> t -> t

module Roll : Dice.S -> sig
  val report : t -> report
  val pick : Defs.power -> t -> t
  val sum_report : t -> sum_report
end
