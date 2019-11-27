type kind = Barrage | ClanTrade | ClearSky | NumendorTrade

type t

val empty : t

val has : kind -> t -> bool
val set : kind -> bool -> t -> t
