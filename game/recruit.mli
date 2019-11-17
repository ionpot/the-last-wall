type action = Add | Promote | Train
type pool =
  | Exclude of Pool.kind
  | From of Pool.kind * Units.kind
  | To of Pool.kind

module type Cap = State.S -> sig
  val value : Defs.count option
end

module type Type = sig
  val action : action
  val kind : Units.kind
  val pool : pool option
  module Cap : Cap
end

module Event : Type ->
  Event.Direct with type t = Defs.count

module NoCap : Cap
