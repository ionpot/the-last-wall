module Not (Check : Event.Check) (State : State.S) = struct
  module Result = Check(State)
  let value = not Result.value
end

module NoEnemies (State : State.S) = struct
  let value = State.Enemy.empty ()
end

module HasEnemies = Not(NoEnemies)
