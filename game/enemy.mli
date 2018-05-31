open Defs

type t = Skeleton | Orc | Demon
type count = int
type party
type resource = Resource.t
type vague_report = (count * t list)

val scouting_cost : resource
val t_list : t list

val count_of : party -> count
val type_of : party -> t

val damage : party list -> manpower
val scout : party list -> party list
val spawn : turn -> party list
val vague_scout : party list -> vague_report
