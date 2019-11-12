module Make (S : Map.S) : sig
  val mapk : (S.key -> 'b) -> 'a S.t -> 'b S.t
  val nth : 'a S.t -> int -> S.key
  module Float : sig
    type t = float S.t
    val pick : t -> float -> S.key
  end
end
