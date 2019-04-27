open Defs

val invalid : count list -> 'a

module With : Dice.S -> sig
  val random : power -> (count * power) list -> count list
end
