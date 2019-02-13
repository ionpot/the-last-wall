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

  type t = (event, input, notify) Phase.output
end

module type Steps = Phase.Steps with
  type Output.event = Output.event and
  type Output.input = Output.input and
  type Output.notify = Output.notify and
  type Output.t = Output.t

module Steps : Steps = struct
  module Output = Output

  type cond = BuildSupply
  type direct = Starting | Support
  type input = Build | Nations | Scout
  type notify = unit

  type event = (cond, direct, input, notify) Phase.etype

  type t = event Phase.step list

  let steps =
    let open Phase in
    [ Do (Direct Starting);
      Do (Input Nations);
      Do (Direct Support);
      Do (Input Build);
      Do (Cond BuildSupply);
      Do (Input Scout)
    ]

  let is_end _ = false

  module Apply (S : State.S) = struct
    let event = function
      | Output.BuildSupply x -> S.bld_supp x
      | Output.Starting x -> S.add_res x
      | Output.Support x -> S.add_res (Nation.total_of x)

    let input = function
      | Output.Build x -> S.build x
      | Output.Nations x -> S.set_nats x
      | Output.Scout x -> S.set_scouting x
  end

  module Check = struct
    let cond : cond -> (module Event.Check) = function
      | BuildSupply -> (module Cond.BuildSupply.Check)

    let input : input -> (module Event.Check) = function
      | Build -> (module Input.Build.Check)
      | Nations -> (module Input.Nations.Check)
      | Scout -> (module Input.Scout.Check)

    let notify : notify -> (module Event.Check) = fun () ->
      (module Event.Never)
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
