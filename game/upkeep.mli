open Defs

module Make : State.S -> sig
  val value : supply
end

module Starvation : State.S -> sig
  val value : count * count
end
