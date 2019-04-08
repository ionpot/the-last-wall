type t
type kind = Skeleton | Orc | Demon
type expr = (Defs.count * kind)
type report =
  | Accurate of t
  | Vague of (Defs.count * kind list)

val empty : t
val kinds : kind list

val combine : t -> t -> t
val damage : t -> Defs.power
val discard : Defs.power -> t -> t
val find : expr -> t -> expr
val reduce : expr -> t -> t
val spawn : Defs.turn -> t
val to_count : kind -> t -> Defs.count
val to_report : Defs.scouting -> t -> report
