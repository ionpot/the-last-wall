module Steps = Steps.Phase3

module Input = struct
  module Event = Input
  type event =
    | Barrage of Event.Barrage.t
    | Scout of Event.Scout.t

  module Apply (State : State.S) = struct
    module Apply = Phase.Apply(State)
    let event = function
      | Barrage x -> Apply.value x (module Event.Barrage)
      | Scout x -> Apply.value x (module Event.Scout)
  end
end

module Output = struct
  type event =
    | Attack
    | Ballista of Cond.Ballista.t
    | Barraged of Cond.Barraged.t
    | Combat of Direct.Combat.t
    | Cyclops of Cond.Cyclops.t
    | Defeat
    | LevelUp
    | NoAttack
    | NoEnemies
    | Revive of Cond.Revive.t
    | Smite of Cond.Smite.t
    | Victory
end

module Convert = struct
  module Input = struct
    module Steps = Steps.Input
    module Event = Input.Event
    module Convert = Phase.Convert.Input(Steps)(Input)

    let cond () = failwith "no phase3 input cond"

    let direct : Convert.direct = function
      | Steps.Barrage -> (module struct module Event = Event.Barrage
          let make x = Input.Barrage x end)
      | Steps.Scout -> (module struct module Event = Event.Scout
          let make x = Input.Scout x end)
  end

  module Output = struct
    module Steps = Steps.Output
    module Convert = Phase.Convert.Output(Steps)(Output)

    let check : Convert.check = function
      | Steps.Attack -> (module struct module Check = Check.HasEnemies
          let value = Output.Attack end)
      | Steps.LevelUp -> (module struct module Check = Check.LevelUp
          let value = Output.LevelUp end)
      | Steps.NoAttack -> (module struct module Check = Check.NoEnemies
          let value = Output.NoAttack end)
      | Steps.NoEnemies -> (module struct module Check = Check.NoEnemies
          let value = Output.NoEnemies end)

    let cond : Convert.cond = function
      | Steps.Ballista -> (module struct module Event = Cond.Ballista
          let make x = Output.Ballista x end)
      | Steps.Barraged -> (module struct module Event = Cond.Barraged
          let make x = Output.Barraged x end)
      | Steps.Cyclops -> (module struct module Event = Cond.Cyclops
          let make x = Output.Cyclops x end)
      | Steps.Defeat -> (module struct module Event = Cond.Defeat
          let make () = Output.Defeat end)
      | Steps.Revive -> (module struct module Event = Cond.Revive
          let make x = Output.Revive x end)
      | Steps.Smite -> (module struct module Event = Cond.Smite
          let make x = Output.Smite x end)

    let direct : Convert.direct = function
      | Steps.Combat -> (module struct module Event = Direct.Combat
          let make x = Output.Combat x end)
      | Steps.Victory -> (module struct module Event = Direct.Victory
          let make () = Output.Victory end)
  end
end
