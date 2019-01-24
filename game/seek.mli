type 'a t =
  | This of 'a
  | Next
  | End

module Apply (P : Phase.S) : State.S -> sig
  val value : P.output -> unit
end

module First (P : Phase.S) : State.S -> sig
  val value : P.output
end

module Next (P : Phase.S) : State.S -> sig
  val value : P.output -> P.output t
end
