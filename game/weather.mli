type degree = Light | Heavy
type t =
  | Breeze
  | Clear
  | Cloudy
  | Fog
  | Heat
  | Wind
  | Rain of degree
  | Snow of degree

val empty : t

val is_bad : t -> bool

module Roll : Dice.S -> sig
  val random : Month.t -> t
end
