module Output = struct
  type event =
    | BuildSupply of Cond.BuildSupply.t
    | Starting of Direct.Starting.t
    | Support of Direct.Nation.t

  type input =
    | Build of Input.Build.t
    | Nations of Input.Nations.t
    | Scout of Input.Scout.t

  type notify = unit

  type t = (event, input, notify) Phase.output
end

module S : Phase.S = struct
  module Output = struct include Output end

  type cond = BuildSupply
  type direct = Starting | Support
  type input = Build | Nations | Scout
  type notify = Steps.no_def

  type event = (cond, direct, input, notify) Defs.event

  let steps =
    let open Defs in
    [ Do (Direct Starting);
      Do (Input Nations);
      Do (Direct Support);
      Do (Input Build);
      Do (Cond BuildSupply);
      Do (Input Scouts)
    ]

  module Event = struct
    let of_event = function
      | Output.BuildSupply _ -> Defs.Cond BuildSupply
      | Output.Starting _ -> Defs.Direct Starting
      | Output.Support _ -> Defs.Direct Support
  end

  let cond_check = function
    | BuildSupply -> (module Cond.BuildSupply.Check)

  let input_check = function
    | Build -> (module Input.Build.Check)
    | Nations -> (module Input.Nations.Check)
    | Scout -> (module Input.Scout.Check)

  let nfy_check = Steps.no_fn
end

module S : Phase.S with type output = Output.output = struct
  include Output

  module Steps = Steps

  let of_event = function
    | BuildSupply _ -> Defs.Cond Steps.BuildSupply
    | Starting _ -> Defs.Direct Steps.Starting
    | Support _ -> Defs.Direct Steps.Support

  let of_input = function
    | Build _ -> Defs.Input Steps.Build
    | Nations _ -> Defs.Input Steps.Nations
    | Scout _ -> Defs.Input Steps.Scout

  let of_notify = Phase.no_step

  let apply = function
    | Build x -> M.build x
    | BuildSupply x -> M.bld_supp x
    | End -> ()
    | Nations x -> M.set_nats x
    | SendScouts x -> M.set_scouting x
    | Starting x -> M.add_res x
    | Support x -> M.add_res (Nation.total_of x)

  let check_supp () =
    let cost = M.bld_supp_cost () in
    if cost > 0
    then BuildSupply cost
    else End

  let next = function
    | Starting _ -> Nations (M.get_nats ())
    | Nations _ -> Support (Support.get ())
    | Support _ -> Build []
    | Build _ -> check_supp ()
    | BuildSupply _ -> SendScouts (M.is_scouting ())
    | SendScouts _
    | End -> End
end
