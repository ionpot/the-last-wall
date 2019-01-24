module type S = sig
  type t
  module Apply : State.S -> sig val value : t -> unit end
  module Make : State.S -> sig val value : t end
end

module Support : S with type t = Nation.support list = struct
  type t = Nation.support list
  module Apply (S : State.S) = struct
    let value x = S.add_res @@ Nation.total_of x
  end
  module Make (S : State.S) = struct
    module M = Check_support.Make(S)
    let value = M.get ()
  end
end

module Starting : S with type t = Resource.t = struct
  type t = Resource.t
  module Apply (S : State.S) = struct
    let value x = S.add_res x
  end
  module Make (S : State.S) = struct
    let value = S.get_deity () |> Deity.starting
  end
end
