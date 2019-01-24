type ('evt, 'input) event =
  | Event of 'evt
  | Input of 'input

module type S = sig
  type t = ('a, 'b) event
  type cond
  type event
  type step = (cond, event) Step.t
  type kind = (cond, event) Step.kind

  val steps : step list

  val cond_of : cond -> (module Cond.S)

  val t2kind : t -> kind

  module Apply (S : Game.State.S) : sig
    val value : t -> unit
  end

  module Make (S : Game.State.S) : sig
    val cond : cond -> t
    val evt : event -> t
  end
end
