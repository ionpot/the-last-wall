module Make (S : Map.S) : sig
  val discardk : (S.key -> bool) -> 'a S.t -> 'a S.t
  val filterk : (S.key -> bool) -> 'a S.t -> 'a S.t
  val mapk : (S.key -> 'b) -> 'a S.t -> 'b S.t
  val min : 'a S.t -> 'a
  val nth : 'a S.t -> int -> S.key
  module Float : sig
    type t = float S.t
    val max : t -> float
    val pick : t -> float -> S.key
    val sum : t -> float
  end
  module Int : sig
    type t = int S.t
    val add : t -> t -> t
    val div : t -> t -> t
    val min : t -> int
    val mul_by : int -> t -> t
    val sub : t -> t -> t
    val sum : t -> int
  end
end
