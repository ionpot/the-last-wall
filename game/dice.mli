module type From = sig
  val bool : unit -> bool
  val float : float -> float
  val int : int -> int
end

module type S = sig
  val between : int -> int -> int
  val betweenf : float -> float -> float
  val between_try : int -> int -> int
  val betweenf_try : float -> float -> float
  val betweenf_times : int -> float -> float -> float
  val betweenf_times_try : int -> float -> float -> float
  val chance : float -> bool
  val deviate : int -> int -> int
  val index : int -> int
  val pick : 'a list -> 'a
  val pick_w : float list -> 'a list -> 'a
  val roll : int -> int
  val rollf : float -> float
  val round : float -> int
  val yes : unit -> bool
end

module From : From -> S
