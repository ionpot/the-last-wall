module Check (M : State.S) : sig
  val attacking : Enemy.party list -> Enemy.party option
end
