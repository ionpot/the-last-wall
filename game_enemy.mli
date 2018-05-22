open Game_defs

type count = int
type party
type resource = Game_resource.t

val scouting_cost : resource

val count_of : party -> count
val type_of : party -> enemy

val damage : party list -> manpower
val scout : party list -> party list
val spawn : turn -> party list
val vague_scout : party list -> party list
