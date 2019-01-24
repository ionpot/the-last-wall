module type S = Event.Conditional

module BuildSupply : S with type t = Defs.supply = struct
  type t = Defs.supply
  module Apply (S : State.S) = struct
    let value x = S.bld_supp x
  end
  module Check (S : State.S) = struct
    let value = S.bld_supp_cost () > 0
  end
  module Make (S : State.S) = struct
    let value = S.bld_supp_cost ()
  end
end
