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

val damage : party list -> manpower
val spawn : turn -> party list

val smite : party list -> party option
val reduce : party -> party list -> party list
