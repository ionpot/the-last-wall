type action = New | Promote | Train
type pool =
  | Add of Pool.kind
  | Exclude of Pool.kind
  | From of Pool.kind
  | Set of Pool.kind

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
