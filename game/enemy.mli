open Defs

type t = Skeleton | Orc | Demon
type count = int
type party
type resource = Resource.t

val scouting_cost : resource
val t_list : t list

val count_of : party -> count
val type_of : party -> enemy

val damage : party list -> manpower
val scout : party list -> party list
val spawn : turn -> party list
val vague_scout : party list -> party list
