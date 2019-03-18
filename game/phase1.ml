module Output = struct
  type event =
    | BuildSupply of Cond.BuildSupply.t
    | Starting of Direct.Starting.t
    | Support of Direct.Support.t

  type input =
    | Build of Input.Build.t
    | Nations of Input.Nations.t
    | Scout of Input.Scout.t

  type notify = unit

  module Apply (State : State.S) = struct
    let event = function
      | BuildSupply x -> State.bld_supp x
      | Starting x -> State.add_res x
      | Support x -> State.add_res (Nation.total_of x)

    let input = function
      | Build x -> State.build x
      | Nations x -> State.set_nats x
      | Scout x -> State.set_scouting x
  end
end

module Steps = Steps.Phase1

module type S = Phase.S with
  type Output.event = Output.event and
  type Output.input = Output.input and
  type Output.notify = Output.notify and
  type Steps.cond = Steps.cond and
  type Steps.direct = Steps.direct and
  type Steps.input = Steps.input and
  type Steps.notify = Steps.notify

module S : S = struct
  module Output = Output
  module Steps = Steps

  open Steps

  module Check = struct
    let cond : cond -> (module Event.Check) = function
      | BuildSupply -> (module Cond.BuildSupply.Check)

    let input : input -> (module Event.Check) = function
      | Build -> (module Input.Build.Check)
      | Nations -> (module Input.Nations.Check)
      | Scout -> (module Input.Scout.Check)

    let notify : notify -> (module Event.Check) =
      fun () -> (module Event.Never)
  end

  module Make (S : State.S) = struct
    let cond = let open Cond in function
      | BuildSupply ->
          let module M = BuildSupply.Make(S) in Output.BuildSupply M.value

    let direct = let open Direct in function
      | Starting -> let module M = Starting.Make(S) in Output.Starting M.value
      | Support -> let module M = Support.Make(S) in Output.Support M.value

    let input = let open Input in function
      | Build -> let module M = Build.Make(S) in Output.Build M.value
      | Nations -> let module M = Nations.Make(S) in Output.Nations M.value
      | Scout -> let module M = Scout.Make(S) in Output.Scout M.value

    let notify () = ()
  end
end
