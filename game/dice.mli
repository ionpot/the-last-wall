module type From = sig
  val bool : unit -> bool
  val float : float -> float
  val int : int -> int
end

module type S = sig
  val between : int -> int -> int
  val chance : float -> bool
  val deviate : int -> int -> int
  val index : int -> int
  val pick : 'a list -> 'a
  val roll : int -> int
  val rollf : float -> float
  val round : float -> int
  val yes : unit -> bool
end

module From : From -> S
