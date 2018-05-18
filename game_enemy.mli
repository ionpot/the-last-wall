open Game_defs

type count = int
type party

val count_of : party -> count
val type_of : party -> enemy

val spawn : turn -> party list
val damage : party list -> manpower
