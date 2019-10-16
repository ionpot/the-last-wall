type kind = Comet | Disease | Tavern

val kinds : kind list

type t

val empty : t

val has : kind -> t -> bool

module Roll : Dice.S -> sig
  val from : (kind -> bool) -> t
end
