open Defs

type kind = Skeleton | Orc | Demon
type expr = (count * kind)
type report =
  | Accurate of expr list
  | Vague of (count * kind list)
type t

val empty : t
val kinds : kind list

val combine : t -> t -> t
val damage : t -> power
val find : count -> kind -> t -> count
val has : kind -> t -> bool
val reduce : count -> kind -> t -> t
val to_count : kind -> t -> count

module Roll : Dice.S -> sig
  val attack : turn -> t
  val loss : power -> t -> t
  val report : scouting -> t -> report
end
