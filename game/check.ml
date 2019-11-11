module Not (Check : Event.Check) (State : State.S) = struct
  module Result = Check(State)
  let value = not Result.value
end

module Fog (State : State.S) = struct
  let value = State.Weather.is Weather.Fog
end

module NoFog = Not(Fog)

module LevelUp (State : State.S) = struct
  let value = State.Leader.check Leader.lvup
end

module NoEnemies (State : State.S) = struct
  let value = State.Enemy.check Units.is_empty
end

module HasEnemies = Not(NoEnemies)
