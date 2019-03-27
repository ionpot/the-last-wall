open Defs

type t = Skeleton | Orc | Demon
type count = int
type party
type report =
  | Accurate of party list
  | Vague of count * t list

val t_list : t list

val count_of : party -> count
val type_of : party -> t

val make : (count * t) list -> party list
val damage : party list -> float
val spawn : turn -> party list

val to_report : party list -> scouting -> report

val find : count -> t -> party list -> party option
val reduce : party -> party list -> party list
