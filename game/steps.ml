module type S = sig
  type cond and direct and input and notify
  type event = (cond, direct, input, notify) Defs.event
  val all : event Defs.step list
  val cond_check : cond -> (module Event.Check)
  val input_check : input -> (module Event.Check)
  val nfy_check : notify -> (module Event.Check)
end
