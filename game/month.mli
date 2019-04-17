type t = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec

val empty : t

val next : t -> t

module Roll : Dice.S -> sig
  val random : unit -> t
end
