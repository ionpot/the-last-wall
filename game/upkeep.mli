open Defs

module Check (M: State.S) : sig
  val total : manpower
end

module Starvation (M: State.S) : sig
  val value : (manpower * manpower) option
end
