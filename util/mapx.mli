module Make (S : Map.S) : sig
  val mapk : (S.key -> 'b) -> 'a S.t -> 'b S.t
  val min : 'a S.t -> 'a
  val nth : 'a S.t -> int -> S.key
  module Float : sig
    type t = float S.t
    val max : t -> float
    val pick : t -> float -> S.key
    val sum : t -> float
  end
end
