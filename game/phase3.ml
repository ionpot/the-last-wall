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
    | BadWeather of Cond.BadWeather.t
    | Barraged of Cond.Barraged.t
    | Combat of Direct.Combat.t
    | Defeat
    | LevelUp of Cond.LevelUp.t
    | NoAttack
    | NoEnemies
    | Smite of Cond.Smite.t
    | Victory
end

module Convert = struct
  module Input = struct
    module Steps = Steps.Input
    module Convert = Phase.Convert.Input(Steps)(Input)

    let cond : Convert.cond = function
      | Steps.Barrage -> (module struct module Event = Event.Barrage
          let make x = Input.Barrage x end)

    let direct : Convert.direct = function
      | Steps.Scout -> (module struct module Event = Event.Scout
          let make x = Input.Scout x end)
  end

  module Output = struct
    module Steps = Steps.Output
    module Convert = Phase.Convert.Output(Steps)(Output)

    let check : Convert.check = function
      | Steps.Attack -> (module struct module Check = Check.HasEnemies
          let value = Output.Attack end)
      | Steps.NoAttack -> (module struct module Check = Check.NoEnemies
          let value = Output.NoAttack end)
      | Steps.NoEnemies -> (module struct module Check = Check.NoEnemies
          let value = Output.NoEnemies end)

    let cond : Convert.cond = function
      | Steps.BadWeather -> (module struct module Event = Cond.BadWeather
          let make x = Output.BadWeather x end)
      | Steps.Barraged -> (module struct module Event = Cond.Barraged
          let make x = Output.Barraged x end)
      | Steps.Defeat -> (module struct module Event = Cond.Defeat
          let make () = Output.Defeat end)
      | Steps.LevelUp -> (module struct module Event = Cond.LevelUp
          let make x = Output.LevelUp x end)
      | Steps.Smite -> (module struct module Event = Cond.Smite
          let make x = Output.Smite x end)

    let direct : Convert.direct = function
      | Steps.Combat -> (module struct module Event = Direct.Combat
          let make x = Output.Combat x end)
      | Steps.Victory -> (module struct module Event = Direct.Victory
          let make () = Output.Victory end)
  end
end
