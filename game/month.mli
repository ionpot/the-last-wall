type t = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec

val empty : t

val is_winter : t -> bool

val next : t -> t

module Roll : sig
  val random : unit -> t
end
