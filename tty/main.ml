module Main = Game.Main
module Phases = Game.Phases
module S = Game.State.Make(Random)
module M = Main.Make(S)

module type Steps = sig
  module Phase : Game.Phase.S
  module Handler : sig
    val input : Phase.Input.event -> Phase.Input.event
    val output : Phase.Output.event -> unit
  end
end

module Step (Steps : Steps) = struct
  module Handle = Steps.Handler
  module Step = Game.Step.Of(Steps.Phase)

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

(*
let status =
  let get () = S.Supply.get (), S.Units.get () in
  let x = ref (get ()) in
  fun () ->
    let y = get () in
    if !x <> y then begin
      x := y;
      Tty.pairln "status" (status2str y)
    end
*)

let map = function
  | Phases.Ph1 (evt, steps) -> Phases.Ph1 (Phase1.map evt, steps)
  | Phases.Ph2 (evt, steps) -> Phases.Ph2 (Phase2.map evt, steps)
  | Phases.Ph3 (evt, steps) -> Phases.Ph3 (Phase3.map evt, steps)

let rec check = function
  | Main.Event evt -> Main.Event (map evt) |> M.next |> after
  | Main.End -> Tty.fin ()

and after evt =
  (*status ();*)
  Tty.flush ();
  check evt

let () =
  Random.self_init ();
  M.first () |> check
