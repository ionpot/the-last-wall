open Defs

type resource = Resource.t
type t = (nation * resource option)

val of_nation : nation -> t
val of_nats : nation list -> t list
val total_of : t list -> resource
