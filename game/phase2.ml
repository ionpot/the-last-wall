module Steps = Steps.Phase2

module Input = struct
  module Event = Input

  type event =
    | Ballista of Event.Ballista.t
    | Barracks of Event.Barracks.t
    | BarrageTrain of Event.BarrageTrain.t
    | Berserker of Event.Berserker.t
    | Build of Event.BuildAvlb.t
    | Dervish of Event.Dervish.t
    | Harcher of Event.Harcher.t
    | Knight of Event.Knight.t
    | LeaderNew of Event.LeaderNew.t
    | Mercs of Event.Mercs.t
    | Nations of Event.Nations.t
    | Novice of Event.Novice.t
    | Ranger of Event.Ranger.t
    | Research of Event.Research.t
    | Sodistan of Event.Sodistan.t
    | Templar of Event.Templar.t
    | Temple of Event.Temple.t
    | Trade of Event.Trade.t
    | Veteran of Event.Veteran.t
    | Volunteers of Event.Volunteers.t

  module Apply (State : State.S) = struct
    module Apply = Phase.Apply(State)
    let event = function
      | Ballista x -> Apply.value x (module Event.Ballista)
      | Barracks x -> Apply.value x (module Event.Barracks)
      | BarrageTrain x -> Apply.value x (module Event.BarrageTrain)
      | Berserker x -> Apply.value x (module Event.Berserker)
      | Build x -> Apply.value x (module Event.BuildAvlb)
      | Dervish x -> Apply.value x (module Event.Dervish)
      | Harcher x -> Apply.value x (module Event.Harcher)
      | Knight x -> Apply.value x (module Event.Knight)
      | LeaderNew x -> Apply.value x (module Event.LeaderNew)
      | Mercs x -> Apply.value x (module Event.Mercs)
      | Nations x -> Apply.value x (module Event.Nations)
      | Novice x -> Apply.value x (module Event.Novice)
      | Ranger x -> Apply.value x (module Event.Ranger)
      | Research x -> Apply.value x (module Event.Research)
      | Sodistan x -> Apply.value x (module Event.Sodistan)
      | Templar x -> Apply.value x (module Event.Templar)
      | Temple x -> Apply.value x (module Event.Temple)
      | Trade x -> Apply.value x (module Event.Trade)
      | Veteran x -> Apply.value x (module Event.Veteran)
      | Volunteers x -> Apply.value x (module Event.Volunteers)
  end
end

module Output = struct
  type event =
    | Attack of Direct.Attack.t
    | Blessing of Direct.Blessing.t
    | BuildManp of Direct.BuildManp.t
    | BuildStatus of Direct.BuildStatus.t
    | BuildSupply of Direct.BuildSupply.t
    | Cavalry of Direct.Cavalry.t
    | Defeat
    | Disease of Cond.Disease.t
    | Facilities of Direct.Facilities.t
    | FearEnd of Direct.FearEnd.t
    | Mishap of Direct.Mishap.t
    | ResearchProgress of Direct.ResearchProgress.t
    | ResearchStatus of Direct.ResearchStatus.t
    | Starvation of Direct.Starvation.t
    | Support of Direct.NationSupport.t
    | Turn of Direct.Turn.t
    | Upkeep of Direct.Upkeep.t
end

module Convert = struct
  module Input = struct
    module Event = Input.Event
    module Steps = Steps.Input
    module Convert = Phase.Convert.Input(Steps)(Input)

    let direct : Convert.direct = function
      | Steps.Ballista -> (module struct module Event = Event.Ballista
          let make x = Input.Ballista x end)
      | Steps.BarrageTrain -> (module struct module Event = Event.BarrageTrain
          let make x = Input.BarrageTrain x end)
      | Steps.Berserker -> (module struct module Event = Event.Berserker
          let make x = Input.Berserker x end)
      | Steps.Build -> (module struct module Event = Event.BuildAvlb
          let make x = Input.Build x end)
      | Steps.Dervish -> (module struct module Event = Event.Dervish
          let make x = Input.Dervish x end)
      | Steps.Knight -> (module struct module Event = Event.Knight
          let make x = Input.Knight x end)
      | Steps.Mercs -> (module struct module Event = Event.Mercs
          let make x = Input.Mercs x end)
      | Steps.Nations -> (module struct module Event = Event.Nations
          let make x = Input.Nations x end)
      | Steps.Novice -> (module struct module Event = Event.Novice
          let make x = Input.Novice x end)
      | Steps.Research -> (module struct module Event = Event.Research
          let make x = Input.Research x end)
      | Steps.Sodistan -> (module struct module Event = Event.Sodistan
          let make x = Input.Sodistan x end)

    let cond : Convert.cond = function
      | Steps.Barracks -> (module struct module Event = Event.Barracks
          let make x = Input.Barracks x end)
      | Steps.Harcher -> (module struct module Event = Event.Harcher
          let make x = Input.Harcher x end)
      | Steps.LeaderNew -> (module struct module Event = Event.LeaderNew
          let make x = Input.LeaderNew x end)
      | Steps.Ranger -> (module struct module Event = Event.Ranger
          let make x = Input.Ranger x end)
      | Steps.Templar -> (module struct module Event = Event.Templar
          let make x = Input.Templar x end)
      | Steps.Temple -> (module struct module Event = Event.Temple
          let make x = Input.Temple x end)
      | Steps.Trade -> (module struct module Event = Event.Trade
          let make x = Input.Trade x end)
      | Steps.Veteran -> (module struct module Event = Event.Veteran
          let make x = Input.Veteran x end)
      | Steps.Volunteers -> (module struct module Event = Event.Volunteers
          let make x = Input.Volunteers x end)
  end

  module Output = struct
    module Steps = Steps.Output
    module Convert = Phase.Convert.Output(Steps)(Output)

    let check () = failwith "no phase2 output check"

    let cond : Convert.cond = function
      | Steps.Defeat -> (module struct module Event = Cond.Defeat
          let make () = Output.Defeat end)
      | Steps.Disease -> (module struct module Event = Cond.Disease
          let make x = Output.Disease x end)

    let direct : Convert.direct = function
      | Steps.Attack -> (module struct module Event = Direct.Attack
          let make x = Output.Attack x end)
      | Steps.Blessing -> (module struct module Event = Direct.Blessing
          let make x = Output.Blessing x end)
      | Steps.BuildManp -> (module struct module Event = Direct.BuildManp
          let make x = Output.BuildManp x end)
      | Steps.BuildStatus -> (module struct module Event = Direct.BuildStatus
          let make x = Output.BuildStatus x end)
      | Steps.BuildSupply -> (module struct module Event = Direct.BuildSupply
          let make x = Output.BuildSupply x end)
      | Steps.Cavalry -> (module struct module Event = Direct.Cavalry
          let make x = Output.Cavalry x end)
      | Steps.Facilities -> (module struct module Event = Direct.Facilities
          let make x = Output.Facilities x end)
      | Steps.FearEnd -> (module struct module Event = Direct.FearEnd
          let make x = Output.FearEnd x end)
      | Steps.Mishap -> (module struct module Event = Direct.Mishap
          let make x = Output.Mishap x end)
      | Steps.ResearchProgress -> (module struct module Event = Direct.ResearchProgress
          let make x = Output.ResearchProgress x end)
      | Steps.ResearchStatus -> (module struct module Event = Direct.ResearchStatus
          let make x = Output.ResearchStatus x end)
      | Steps.Starvation -> (module struct module Event = Direct.Starvation
          let make x = Output.Starvation x end)
      | Steps.Support -> (module struct module Event = Direct.NationSupport
          let make x = Output.Support x end)
      | Steps.Turn -> (module struct module Event = Direct.Turn
          let make x = Output.Turn x end)
      | Steps.Upkeep -> (module struct module Event = Direct.Upkeep
          let make x = Output.Upkeep x end)
  end
end
