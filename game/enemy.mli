open Defs

type t = Skeleton | Orc | Demon
type count = int
type party
type resource = Resource.t

val t_list : t list

val count_of : party -> count
val type_of : party -> t

val damage : party list -> manpower
val spawn : turn -> party list

val smite : party list -> party option
val reduce : party -> party list -> party list
