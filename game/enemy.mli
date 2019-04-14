open Defs

type t
type kind = Skeleton | Orc | Demon
type expr = (count * kind)
type report =
  | Accurate of expr list
  | Vague of (count * kind list)

val empty : t
val kinds : kind list

val combine : t -> t -> t
val damage : t -> power
val discard : power -> t -> t
val find : count -> kind -> t -> count
val reduce : count -> kind -> t -> t
val spawn : turn -> t
val to_count : kind -> t -> count
val to_report : scouting -> t -> report
