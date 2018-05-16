open Game_defs

type resource = Game_resource.t
type t = (nation * resource option)

val of_nation : nation -> t
val of_list : nation list -> t list
val total_of : t list -> resource
