module Output = struct
  type event =
    | BuildSupply of Direct.BuildSupply.t
    | Starting of Direct.Starting.t
    | Support of Direct.Support.t

  type input =
    | Build of Input.Build.t
    | Nations of Input.Nations.t
    | Scout of Input.Scout.t

  type notify = unit

  module Apply (State : State.S) = struct
    module Apply = Event.Apply(State)

    let event = function
      | BuildSupply x -> Apply.value x (module Direct.BuildSupply)
      | Starting x -> Apply.value x (module Direct.Starting)
      | Support x -> Apply.value x (module Direct.Support)

    let input = function
      | Build x -> Apply.value x (module Input.Build)
      | Nations x -> Apply.value x (module Input.Nations)
      | Scout x -> Apply.value x (module Input.Scout)
  end
end

module S = struct
  module Output = Output
  module Steps = Steps.Phase1

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
