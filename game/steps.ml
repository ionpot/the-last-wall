type ('c, 'd, 'i, 'n) etype =
  | Cond of 'c
  | Direct of 'd
  | Input of 'i
  | Notify of 'n

type 'a step =
  | Do of 'a
  | Either of ('a * 'a)
  | JumpIfNo of ('a * 'a)

module type S = sig
  type cond and direct and input and notify
  type event = (cond, direct, input, notify) etype
  type t = event step list
  val list : t
end

module Phase1 = struct
  type cond = BuildSupply
  type direct = Starting | Support
  type input = Build | Nations | Scout
  type notify = unit

  type event = (cond, direct, input, notify) etype
  type t = event step list

  let list =
    [ Do (Direct Starting);
      Do (Input Nations);
      Do (Direct Support);
      Do (Input Build);
      Do (Cond BuildSupply);
      Do (Input Scout)
    ]
end

module Phase2 = struct
  type cond = BuildSupply
  type direct = Starting | Support
  type input = Build | Nations | Scout
  type notify = unit

  type event = (cond, direct, input, notify) etype
  type t = event step list

  let list =
    [ Do (Direct Starting);
      Do (Input Nations);
      Do (Direct Support);
      Do (Input Build);
      Do (Cond BuildSupply);
      Do (Input Scout)
    ]
end

module Phase3 = struct
  type cond = BuildSupply
  type direct = Starting | Support
  type input = Build | Nations | Scout
  type notify = unit

  type event = (cond, direct, input, notify) etype
  type t = event step list

  let list =
    [ Do (Direct Starting);
      Do (Input Nations);
      Do (Direct Support);
      Do (Input Build);
      Do (Cond BuildSupply);
      Do (Input Scout)
    ]
end
