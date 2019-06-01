module Main = Game.Main
module Phases = Game.Phases
module S = Game.State.Make(Random)
module M = Main.Make(S)
module Status' = Status.With(S)

module type Steps = sig
  module Phase : Game.Phase.S
  module Handler : sig
    module Make : Game.State.S -> sig
      val input : Phase.Input.event -> Phase.Input.event
      val output : Phase.Output.event -> unit
    end
    module After : Status.S -> sig
      val input : Phase.Input.event -> unit
      val output : Phase.Output.event -> unit
    end
  end
end

module Step (Steps : Steps) = struct
  module After = Steps.Handler.After(Status')
  module Handle = Steps.Handler.Make(S)
  module Step = Game.Step.Of(Steps.Phase)

  let after = function
    | Step.Input evt -> After.input evt
    | Step.Output (evt, _) -> After.output evt

  let map = function
    | Step.Input evt -> Step.Input (Handle.input evt)
    | Step.Output (evt, _) as step -> Handle.output evt; step
end

module Phase1 = Step(struct
  module Phase = Game.Phase1
  module Handler = Phase1
end)

module Phase2 = Step(struct
  module Phase = Game.Phase2
  module Handler = Phase2
end)

module Phase3 = Step(struct
  module Phase = Game.Phase3
  module Handler = Phase3
end)

let after = function
  | Phases.Ph1 (evt, _) -> Phase1.after evt
  | Phases.Ph2 (evt, _) -> Phase2.after evt
  | Phases.Ph3 (evt, _) -> Phase3.after evt

let map = function
  | Phases.Ph1 (evt, steps) -> Phases.Ph1 (Phase1.map evt, steps)
  | Phases.Ph2 (evt, steps) -> Phases.Ph2 (Phase2.map evt, steps)
  | Phases.Ph3 (evt, steps) -> Phases.Ph3 (Phase3.map evt, steps)

let rec check = function
  | Main.Event evt ->
      let evt' = map evt in
      Main.Event evt' |> M.next |> do_next evt'
  | Main.End -> Tty.fin ()

and do_next prev evt =
  after prev;
  Tty.flush ();
  check evt

let () =
  Random.self_init ();
  M.first () |> check
