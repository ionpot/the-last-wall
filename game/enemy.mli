open Defs

type t = Skeleton | Orc | Demon
type count = int
type party
type report = party list
type sum_report = (count * t list)

val t_list : t list

val count_of : party -> count
val type_of : party -> t

val report_of : party list -> report
val sum_report_of : party list -> sum_report

val make : (count * t) list -> party list
val damage : party list -> float
val spawn : turn -> party list

val find : count -> t -> party list -> party option
val reduce : party -> party list -> party list
