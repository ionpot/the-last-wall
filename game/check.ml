module Not (Check : Event.Check) (State : State.S) = struct
  module Result = Check(State)
  let value = not Result.value
end

module HasEnemies (State : State.S) = struct
  let value = State.has_enemies ()
end

module NoEnemies = Not(HasEnemies)
