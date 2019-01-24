module Of (M : Phase.S) : sig
  val first : M.kind
  val next_of : M.kind -> M.kind option

  module Apply (S : Game.State.S) : sig
    val value : M.t -> unit
  end

  module Make (S : Game.State.S) : sig
    val value : M.kind -> M.t option
  end
end
