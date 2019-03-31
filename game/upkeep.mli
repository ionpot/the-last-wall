open Defs

module Make (M: State.S) : sig
  val value : manpower
end

module Starvation (M: State.S) : sig
  val value : manpower * manpower
end
